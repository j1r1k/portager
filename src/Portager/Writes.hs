-- |
-- Module      :  Portager.Writes
-- Copyright   :  (C) 2017 Jiri Marsicek
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jiri Marsicek <jiri.marsicek@gmail.com>
--
-- This module provides functionality for serialization of portage configuration to configuration files.
--
{-# LANGUAGE OverloadedStrings #-}
module Portager.Writes where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks)

import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import qualified Data.Set as Set (toAscList)
import Data.Text (Text)
import qualified Data.Text as Text (unlines, unpack)
import qualified Data.Text.IO as Text (writeFile)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, (</>))

import Text.Printf (printf)

import Portager.DSL
import Portager.Flatten (FlatPackage(..), flattenSet)
import Portager.Options (Options(..), WorldSet)

-- |Adds Atom name as a prefix to text given as parameter.
withAtom :: FlatPackage -> Text -> Text
withAtom fp t = showText (_fpAtom fp) <> " " <> t

-- |Creates a text record for package in a format required by @package.use@ file.
toPackageUseRecord :: FlatPackage -> Maybe Text
toPackageUseRecord fp
  | null useflags = Nothing
  | otherwise = Just $ withAtom fp $ showText useflags
  where useflags = Set.toAscList $_fpUseflags fp

-- |Creates a list of records to be written to portage use file @/etc/portage/package.use/*@
-- See 'toPackageUseRecord'
toPackageUse :: [FlatPackage] -> [Text]
toPackageUse = mapMaybe toPackageUseRecord

-- |Creates a text record for a package in a format required by @package.accept_keywords@ file.
toPackageAcceptKeywordRecord :: FlatPackage -> Maybe Text
toPackageAcceptKeywordRecord fp
  | null kws = Nothing
  | otherwise = Just $ withAtom fp $ showText kws
  where kws = Set.toAscList $_fpKeywords fp

-- |Creates a list of lines to be written to portage accept_keywords file @/etc/portage/package.accept_keywords/*@
-- See 'toPackageAcceptKeywordRecord'
toPackageAcceptKeywords :: [FlatPackage] -> [Text]
toPackageAcceptKeywords = mapMaybe toPackageAcceptKeywordRecord

-- |Creates a text record for a package in a format required by @package.license@ files.
toPackageLicenseRecord :: FlatPackage -> Maybe Text
toPackageLicenseRecord fp
  | null licenses = Nothing
  | otherwise = Just $ withAtom fp $ showText licenses
  where licenses = Set.toAscList $_fpLicenses fp

-- |Creates a list of lines to be written to a portage license file @/etc/portage/package.license@
-- See 'toPackageLicenseRecord'
toPackageLicense :: [FlatPackage] -> [Text]
toPackageLicense = mapMaybe toPackageLicenseRecord

-- |Creates a collection of lines to be written to a portage set file @/etc/portage/sets/*@
toPortageSet :: SetConfiguration -> [Text]
toPortageSet = map (showText . _atom) . _setPackages

data PortageSetConfig = 
  PortageSetConfig 
    { _portageSetName :: Name
    , _portageSet :: [Text]
    , _portagePackageUse :: [Text]
    , _portagePackageAcceptKeywords :: [Text]
    , _portagePackageLicense :: [Text]
    } deriving (Eq, Show)

-- |Converts a 'PackageSet' to 'PortageSetConfig' that can
createPortageSetConfig :: PackageSet -> PortageSetConfig
createPortageSetConfig s = 
  let cfg = _setConfiguration s
      flat = Set.toAscList $ flattenSet s
   in PortageSetConfig 
        { _portageSetName = _setName s
        , _portageSet = toPortageSet cfg
        , _portagePackageUse = toPackageUse flat
        , _portagePackageAcceptKeywords = toPackageAcceptKeywords flat 
        , _portagePackageLicense = toPackageLicense flat
        }

writeLines :: FilePath -> [Text] -> IO ()
writeLines fp = Text.writeFile fp . Text.unlines

-- |Writes text lines to a file enclosed in directory, creating the directory if it does not exist.
-- If there are no lines to be written, no action is performed.
-- Parent of a directory is read from 'Options'.
writePortageSetFile :: FilePath -> FilePath -> [Text] -> ReaderT Options IO ()
writePortageSetFile dir file lns = unless (null lns) $ do
    root <- asks _targetDir
    lift $ createDirectoryIfMissing False (root </> dir)
    lift $ writeLines (root </> dir </> file) lns

-- |Writes a 'PortageSetConfig' with specified index to respective files.
-- @package.use@ file is prefixed with an index, since it is sensitive to order.
writePortageSetConfig :: Int -> PortageSetConfig -> ReaderT Options IO ()
writePortageSetConfig index (PortageSetConfig pName pSet pUseflags pKeywords pLicenses) =
  do writePortageSetFile "sets" name pSet
     writePortageSetFile "package.use" (printf "%02d" index <> name) pUseflags
     writePortageSetFile "package.accept_keywords" name pKeywords
     writePortageSetFile "package.license" name pLicenses
  where name = Text.unpack pName

-- |Performs 'writePortageSetConfig' for WorldSets given as parameter
writePortageSetConfigs :: [WorldSet] -> [PortageSetConfig] -> ReaderT Options IO ()
writePortageSetConfigs ws = mapM_ (uncurry $ writePortageSetConfig) . zip [1..] . filter (\psc -> _portageSetName psc `elem` ws)
