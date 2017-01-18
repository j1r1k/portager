{-# LANGUAGE OverloadedStrings #-}
module Portager.Writes where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks)

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text (unlines, unpack)
import qualified Data.Text.IO as Text (writeFile)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, (</>))

import Text.Printf (printf)

import Portager.Package
import Portager.Flatten (FlatPackage(..), flattenSet)
import Portager.Options (Options(..), WorldSet)

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
    { _portageSetName :: Name
    , _portageSet :: [Text]
    , _portagePackageUse :: [Text]
    , _portagePackageAcceptKeywords :: [Text]
    , _portagePackageLicense :: [Text]
    } deriving (Eq, Show)

createPortageConfig :: Set -> PortageSetConfig
createPortageConfig s = 
  let cfg = _setConfiguration s
      flat = sort $ flattenSet s
   in PortageSetConfig 
        { _portageSetName = _setName s
        , _portageSet = toPortageSet cfg
        , _portagePackageUse = toPortagePackageUse flat
        , _portagePackageAcceptKeywords = toPortagePackageAcceptKeywords flat 
        , _portagePackageLicense = toPortagePackageLicense flat
        }


writeLines :: FilePath -> [Text] -> IO ()
writeLines fp = Text.writeFile fp . Text.unlines

writePortageSetFile :: FilePath -> FilePath -> [Text] -> ReaderT Options IO ()
writePortageSetFile dir file lines = unless (null lines) $ do
    root <- asks _targetDir
    lift $ createDirectoryIfMissing False (root </> dir)
    lift $ writeLines (root </> dir </> file) lines

writePortageSetConfig :: [WorldSet] -> Int -> PortageSetConfig -> ReaderT Options IO ()
writePortageSetConfig ws n (PortageSetConfig pName pSet pUseflags pKeywords pLicenses)
  | pName `elem` ws = do
      writePortageSetFile "sets" name pSet
      writePortageSetFile "package.use" (printf "%02d" n <> name) pUseflags
      writePortageSetFile "package.accept_keywords" name pKeywords
      writePortageSetFile "package.license" name pLicenses
  | otherwise = lift $ putStrLn $ "Skipping set '" <> name <> "'"
  where name = Text.unpack pName

writePortageSetConfigs :: [WorldSet] -> [PortageSetConfig] -> ReaderT Options IO ()
writePortageSetConfigs ws = mapM_ (uncurry $ writePortageSetConfig ws) . zip [1..]
