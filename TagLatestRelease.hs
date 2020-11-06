#! /usr/bin/env bash
{- 2>/dev/null
# vim: set ft=haskell
# Use some stupid polyglot here so that we can add the directory of this script
# as an include path to GHC.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
exec nix-shell \
  "$DIR/shell.nix" \
  --pure \
  --run "runghc -i$(printf "%q " "$DIR" "$0" "$@")"
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TagLatestRelease
  ( main
  ) where

import qualified Control.Category              as C
import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.ByteString.Lazy.UTF8      ( toString )
import           Data.Foldable                  ( for_
                                                , toList
                                                )
import           Data.Maybe
import           Data.Version
import           GHC.Generics
import           Interpolate
import           Options.Generic
import           Shh
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO
import           Text.ParserCombinators.ReadP   ( eof
                                                , readP_to_S
                                                )

load SearchPath ["git", "yq"]

data Opts w = Opts
  { tagPrefix :: w ::: Maybe String <?> "prefix for git tag, default \"v\""
  , package
      :: w ::: Maybe FilePath <?> "path to package.yaml, default 'package.yaml'"
  , startRev
      :: w ::: Maybe String <?> "Revision at which to start searching for changes to version, (default 'HEAD')"
  }
  deriving Generic

main :: IO ()
main = do
  Opts {..} <- unwrapRecord "Tag latest release"

  missingExecutables >>= \case
    [] -> pure ()
    xs -> do
      for_ xs $ \x -> sayErr [i|missing executable $x|]
      exitFailure

  let package'   = fromMaybe "package.yaml" package
      startRev'  = fromMaybe "HEAD" startRev
      tagPrefix' = fromMaybe "v" tagPrefix

  unlessM (doesFileExist package') $ die [i|Unable to find $package'|]

  catchFailure (git "rev-parse" "-q" &> devNull)
    $ \_ -> die "This doesn't seem to be a git repository"

  catchFailure (git "rev-parse" "-q" startRev' &> devNull &!> devNull)
    $ \_ -> die [i|Revision $startRev' doesn't exist|]

  catchFailure (git "rev-parse" "-q" (startRev' <> "~") &> devNull &!> devNull)
    $ \_ ->
        die
          [i|This seems to be a shallow repository (Revision $startRev'~ doesn't exist)|]

  let
    getVersion rev = do
      ver <-
        (  git "show" [i|$rev:$package'|]
          |> yq "--exit-status" "--raw-output" ".version"
          |> captureString
          )
          `catchFailure` \_ -> die
                           [i|unable to get version from $package' (at $rev)|]
      maybe
        (die [i|version from $package' (at $rev) isn't a valid version: $ver|])
        pure
        (readVersion ver)

  -- This is the version we want to find the first appearance of on the main
  -- branch (the one followed by --first-parent)
  currentVersion <- getVersion startRev'
  let tag = tagPrefix' <> showVersion currentVersion

  -- Exit if this version already has a tag
  catchSuccess (git "rev-parse" "-q" "--verify" [i|refs/tags/$tag|] &> devNull)
    $ \_ -> do
        sayErr [i|Tag $tag already exists|]
        liftIO exitSuccess

  -- The current candidate for the package.yaml changing commit
  prev         <- git "rev-parse" "--short" startRev' |> captureString

  -- - Get all the comments on the main branch which touch this file, most recent
  --   ones first, starting with the immediate parent
  -- - If the version has changed between this candidate and the parent, return
  --   the candidate
  -- - Otherwise continue into history, using the parent as the new candidate
  firstChangeM <-
    git "log" "--first-parent" "--pretty=%h" (startRev' <> "~") "--" package'
      |> readInputLines
           (whileM prev $ \s commit -> do
             let commitString = toString commit
             oldVersion <- getVersion commitString
             pure $ if oldVersion /= currentVersion
               then Right s
               else Left commitString
           )

  let firstChange = fromMaybe prev firstChangeM

  sayErr [i|Tagging $firstChange as $tag|]
  git "tag" tag firstChange

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

instance ParseRecord (Opts Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
    { shortNameModifier = firstLetter
    }

die :: MonadIO m => String -> m a
die m = liftIO $ do
  sayErr m
  exitFailure

sayErr :: MonadIO m => String -> m ()
sayErr = liftIO . hPutStrLn stderr

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb a = mb >>= \b -> unless b a

whileM
  :: (Traversable t, Monad m)
  => s
  -- ^ Initial loop state
  -> (s -> a -> m (Either s b))
  -- ^ Loop function, Left s to keep going, Right r to terminate with that result
  -> t a
  -- ^ Values to loop over
  -> m (Maybe b)
  -- ^ The result if the loop terminated
whileM i m =
  let go s = \case
        []     -> pure Nothing
        x : xs -> m s x >>= \case
          Left  s' -> go s' xs
          Right r  -> pure (Just r)
  in  go i . toList

captureString :: Proc String
captureString = toString <$> captureTrim

readVersion :: String -> Maybe Version
readVersion = readP_to_S (parseVersion <* eof) C.>>> \case
  [(a, "")] -> Just a
  _         -> Nothing

catchSuccess :: (Monad m, Shell m) => Proc a -> (a -> Proc ()) -> m ()
catchSuccess p c = tryFailure p >>= \case
  Left  _ -> pure ()
  Right r -> runProc (c r)
