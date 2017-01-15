{-# LANGUAGE OverloadedStrings #-}
module Writes where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)

import Package
import Flatten

withAtom :: FlatPackage -> Text -> Text
withAtom fp t = showText (_fpAtom fp) <> " " <> t

toPackageUse :: FlatPackage -> Maybe Text
toPackageUse fp
  | null useflags = Nothing
  | otherwise = Just $ withAtom fp $ showText (sort useflags)
  where useflags = _fpUseflags fp

toPortagePackageUse :: [FlatPackage] -> [Text]
toPortagePackageUse = mapMaybe toPackageUse

toPackageAcceptKeywords :: FlatPackage -> Maybe Text
toPackageAcceptKeywords fp
  | null keywords = Nothing
  | otherwise = Just $ withAtom fp $ showText keywords
  where keywords = _fpKeywords fp

toPortagePackageAcceptKeywords :: [FlatPackage] -> [Text]
toPortagePackageAcceptKeywords = mapMaybe toPackageAcceptKeywords

toPackageLicense :: FlatPackage -> Maybe Text
toPackageLicense fp
  | null licenses = Nothing
  | otherwise = Just $ withAtom fp $ showText licenses
  where licenses = _fpLicenses fp

toPortagePackageLicense :: [FlatPackage] -> [Text]
toPortagePackageLicense = mapMaybe toPackageLicense

toPortageSet :: SetConfiguration -> [Text]
toPortageSet = map (showText . _atom) . _setPackages

data PortageSetConfig = 
  PortageSetConfig 
    { portageSetName :: Name
    , portageSet :: [Text]
    , portagePackageUse :: [Text]
    , portagePackageAcceptKeywords :: [Text]
    , portagePackageLicense :: [Text]
    } deriving (Eq, Show)

createPortageConfig :: Set -> PortageSetConfig
createPortageConfig s = 
  let cfg = _setConfiguration s
      flat = sort $ flattenSet s
   in PortageSetConfig 
        { portageSetName = _setName s
        , portageSet = toPortageSet cfg
        , portagePackageUse = toPortagePackageUse flat
        , portagePackageAcceptKeywords = toPortagePackageAcceptKeywords flat 
        , portagePackageLicense = toPortagePackageLicense flat
        }