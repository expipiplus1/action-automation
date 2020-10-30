#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p haskellPackages.hpack jq yq git "haskell.packages.ghc865.ghcWithPackages (hp: with hp; [ shh optparse-generic ])"

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

module BumpVersion where

import qualified Control.Category              as C
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Data.ByteString.Lazy.UTF8      ( fromString
                                                , toString
                                                )
import           Data.Foldable                  ( for_ )
import           Data.Maybe
import           Data.Version
import           GHC.Generics
import           Options.Applicative
import           Options.Generic
import           Shh
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           System.IO
import           Text.ParserCombinators.ReadP   ( eof
                                                , readP_to_S
                                                )

load SearchPath ["git", "yq", "cat", "sed", "date", "hpack", "awk"]

data Opts w = Opts
  { version :: w ::: Version <?> "Version to bump package to"
  , packageDir
      :: w ::: Maybe FilePath <?> "Directory containing package.yaml and optionally changelog.md"
  , tagPrefix   :: w ::: Maybe String <?> "prefix for git tag, default \"v\""
  , ignoreDirty :: w ::: Bool <?> "proceed despite a dirty git tree"
  }
  deriving Generic

deriving instance Show (Opts Unwrapped)

instance ParseRecord (Opts Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
    { shortNameModifier = firstLetter
    }

instance ParseRecord Version where
instance ParseFields Version where
instance ParseField Version where
  readField = maybeReader readVersion
readVersion :: String -> Maybe Version
readVersion = readP_to_S (parseVersion <* eof) C.>>> \case
  [(a, "")] -> Just a
  _         -> Nothing

main :: IO ()
main = do
  Opts {..} <- unwrapRecord "Bump versions"

  missingExecutables >>= \case
    [] -> pure ()
    xs -> do
      for_ xs $ \x -> sayErr ("missing executable " <> x)
      exitFailure

  unless ignoreDirty $ do
    changedFiles <-
      git "status" "--short" "--untracked-files=no" |> captureLines
    unless (null changedFiles) $ do
      die
        "There are untracked changes in the working tree, please resolve these before making a release"

  let packageDir' = fromMaybe "." packageDir
      package     = packageDir' </> "package.yaml"
      changelog   = packageDir' </> "changelog.md"
  name       <- captureString <| yq "--raw-output" ".name" <| cat package
  oldVersion <-
    maybe (die ("Couldn't parse version field from " <> package)) pure
    .   readVersion
    =<< captureString
    <|  yq "--raw-output" ".version"
    <|  cat package

  when (oldVersion == version) $ die
    (  "The package is already at the requested version ("
    <> showVersion version
    <> ")"
    )

  when (oldVersion > version) $ die
    (  "The package is already newer than the requested version ("
    <> showVersion version
    <> ")"
    )

  sayErr
    $  "Bumping version of "
    <> name
    <> " from "
    <> showVersion oldVersion
    <> " to "
    <> showVersion version

  sed "-i.bak"
      "-r"
      ("s/^(version: *).*/\\1" <> showVersion version <> "/")
      package
  git "add" package

  doesFileExist changelog >>= \case
    True -> do
      d <- captureString <| date "--iso-8601"
      sed
        "-i.bak"
        ("s/^## WIP$/\\0\\n\\n## [" <> showVersion version <> "] - " <> d <> "/"
        )
        changelog
      git "add" changelog
    False -> sayErr $ changelog <> " not found, not updating"

  hpack packageDir'
  git "add" (packageDir' </> name <.> "cabal")

  let tag    = fromMaybe "v" tagPrefix <> showVersion version
      branch = "release-" <> tag
  git "checkout" "-b" branch

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

  git "commit" "--file" "-" <<< fromString commitMessage

die :: String -> IO a
die m = do
  sayErr m
  exitFailure

sayErr :: String -> IO ()
sayErr = hPutStrLn stderr

captureString :: Proc String
captureString = toString <$> captureTrim
