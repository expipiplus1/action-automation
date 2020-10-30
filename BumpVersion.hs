#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p haskellPackages.hpack jq yq git "haskellPackages.ghcWithPackages (hp: with hp; [ shh optparse-generic ])"

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module BumpVersion
  ( main
  ) where

import qualified Control.Category              as C
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Data.ByteString.Lazy.UTF8      ( fromString
                                                , toString
                                                )
import           Data.Char                      ( toLower )
import           Data.Foldable                  ( for_ )
import           Data.Maybe
import           Data.Version
import           GHC.Generics
import           Options.Applicative
import           Options.Generic
import           Shh
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           System.IO
import           Text.ParserCombinators.ReadP   ( eof
                                                , readP_to_S
                                                )
import           Data.Traversable               ( for )
import           Data.List                      ( sortOn
                                                , groupBy
                                                , find
                                                )
import           Data.Function                  ( on )

load SearchPath ["git", "yq", "cat", "sed", "date", "hpack", "awk"]

data Opts w = Opts
  { version
      :: w ::: [(Maybe String, Bump)] <?> "'patch', 'minor' (default), 'major', 'supermajor' or specify a version explicitly instead of bumping to the next major/minor version. For the multi-update case specify package name by prefixing 'name:'"
  , packageDir
      :: w ::: Maybe FilePath <?> "Directory containing package.yaml and optionally changelog.md, default '.'."
  , tagPrefix   :: w ::: Maybe String <?> "prefix for git tag, default \"v\""
  , ignoreDirty :: w ::: Bool <?> "proceed despite a dirty git tree"
  , dryRun      :: w ::: Bool <?> "do not modify files or make any git changes"
  , dontCommit  :: w ::: Bool <?> "do not 'git commit' changes"
  , packageInfo
      :: w ::: [[PackageInfo]] <?> "Lines containing 'name tag-prefix package-dir' for at least all the packaged specified with --version"
  }
  deriving Generic

data Bump = Patch | Minor | Major | SuperMajor | Explicit Version
  deriving Show

data PackageInfo = PackageInfo String String FilePath
  deriving Show

main :: IO ()
main = do
  Opts {..} <- unwrapRecord "Bump versions"

  missingExecutables >>= \case
    [] -> pure ()
    xs -> do
      for_ xs $ \x -> sayErr ("missing executable " <> x)
      exitFailure

  let version' = if null version then [(Nothing, Minor)] else version
  updates <- for version' $ \case
    (Nothing, b) -> pure (fromMaybe "." packageDir, fromMaybe "v" tagPrefix, b)
    (Just n , b) -> case find ((== n) . packageInfoName) (concat packageInfo) of
      Nothing ->
        die $ "Package " <> n <> " was not specified in --package-info"
      Just (PackageInfo _ tag dir) -> pure (dir, tag, b)

  commits <- for updates $ \(packageDir, tagPrefix, bump) -> do
    let package   = packageDir </> "package.yaml"
        changelog = packageDir </> "changelog.md"

    let git' :: Cmd
        git' = git "-C" packageDir

    unless ignoreDirty $ do
      changedFiles <-
        git' "status" "--short" "--untracked-files=no" |> captureLines
      unless (null changedFiles) $ do
        die
          "There are untracked changes in the working tree, please resolve these before making a release"

    name       <- captureString <| yq "--raw-output" ".name" <| cat package
    oldVersion <-
      maybe (die ("Couldn't parse version field from " <> package)) pure
      .   readVersion
      =<< captureString
      <|  yq "--raw-output" ".version"
      <|  cat package

    let newVersion = bumpVersion oldVersion bump

    when (oldVersion == newVersion) $ die
      (  "The package is already at the requested version ("
      <> showVersion newVersion
      <> ")"
      )

    when (oldVersion > newVersion) $ die
      (  "The package is already newer than the requested version ("
      <> showVersion newVersion
      <> ")"
      )

    sayErr
      $  "Bumping version of "
      <> name
      <> " from "
      <> showVersion oldVersion
      <> " to "
      <> showVersion newVersion

    let tag = tagPrefix <> showVersion newVersion
    commitMessage <-
      let header = tag <> "\n\n"
      in  doesFileExist changelog >>= \case
            True ->
              (header <>)
                <$> captureString
                <|  sed "/^##/d"
                <|  awk "/## WIP/{flag=0;next};/##/{flag=flag+1};flag==1"
                <|  cat changelog
            False -> pure header

    when dryRun $ do
      sayErr "Not making any changes due to dry-run"
      sayErr
        $  "Otherwise would commit with this changelog:\n\n"
        <> commitMessage
      exitSuccess

    sed "-i.bak"
        "-r"
        ("s/^(version: *).*/\\1" <> showVersion newVersion <> "/")
        package
    git' "add" package

    doesFileExist changelog >>= \case
      True -> do
        d <- captureString <| date "--iso-8601"
        sed
          "-i.bak"
          (  "s/^## WIP$/\\0\\n\\n## ["
          <> showVersion newVersion
          <> "] - "
          <> d
          <> "/"
          )
          changelog
        git' "add" changelog
      False -> sayErr $ changelog <> " not found, not updating"

    hpack packageDir
    git' "add" (packageDir </> name <.> "cabal")

    gitRoot <- captureString <| git' "rev-parse" "--show-toplevel"
    pure (gitRoot, commitMessage)

  unless dontCommit
    $ for_ (groupBy ((==) `on` fst) . sortOn fst $ commits)
    $ \xs ->
        let (root : _, messages) = unzip xs
        in  git "-C" root "commit" "--file" "-"
              <<< fromString (unlines messages)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

bumpVersion :: Version -> Bump -> Version
bumpVersion (Version v _) = \case
  Explicit w -> w
  Patch      -> bumpComponent 3 v
  Minor      -> bumpComponent 2 v
  Major      -> bumpComponent 1 v
  SuperMajor -> bumpComponent 0 v
 where
  minNumComponents = 2
  bumpComponent n vs =
    let zs       = vs <> repeat 0
        existing = zs !! n
    in  makeVersion $ take n zs <> [succ existing] <> replicate
          (minNumComponents - n - 1)
          0

deriving instance Show (Opts Unwrapped)

instance ParseRecord (Opts Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
    { shortNameModifier = firstLetter
    }

instance ParseField [PackageInfo] where
  readField = maybeReader readPackageInfos
  metavar _ = "\"NAME TAG_PREFIX PATH\""

instance ParseField (Maybe String, Bump) where
  readField = maybeReader $ splitLast ':' C.>>> \case
    Left  b      -> (Nothing, ) <$> readBump b
    Right (n, b) -> (Just n, ) <$> readBump b
  metavar _ = "[NAME:](patch|minor|major|supermajor|VERSION)"

instance ParseField Bump where
  readField = maybeReader readBump

readBump = fmap toLower C.>>> \case
  "patch"      -> Just Patch
  "minor"      -> Just Minor
  "major"      -> Just Major
  "supermajor" -> Just SuperMajor
  v            -> Explicit <$> readVersion v

readVersion :: String -> Maybe Version
readVersion = readP_to_S (parseVersion <* eof) C.>>> \case
  [(a, "")] -> Just a
  _         -> Nothing

die :: String -> IO a
die m = do
  sayErr m
  exitFailure

sayErr :: String -> IO ()
sayErr = hPutStrLn stderr

captureString :: Proc String
captureString = toString <$> captureTrim

readPackageInfos :: String -> Maybe [PackageInfo]
readPackageInfos s = for (filter (not . null) (lines s)) $ \l ->
  case words l of
    [n, p, d] -> Just (PackageInfo n p d)
    _         -> Nothing

packageInfoName = \case
  PackageInfo n _ _ -> n

-- >>> splitLast ':' "hello:world"
-- Right ("hello","world")
--
-- >>> splitLast ':' "hello,world"
-- Left "hello,world"
--
-- >>> splitLast ':' "foo:hello:world"
-- Right ("foo:hello","world")
--
-- >>> splitLast ':' ""
-- Left ""
--
-- >>> splitLast ':' ":"
-- Right ("","")
splitLast :: Eq a => a -> [a] -> Either [a] ([a], [a])
splitLast s = foldr
  (\c -> \case
    Right (ls, rs) -> Right (c : ls, rs)
    Left rs | c == s    -> Right ([], rs)
            | otherwise -> Left (c : rs)
  )
  (Left [])