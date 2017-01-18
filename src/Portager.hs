{-# LANGUAGE OverloadedStrings #-}
module Portager (
    PortageR
  , portager
  , amd64
  , x86
  , module Portager.Package
) where

import Portager.Package
import Portager.Options
import Portager.Writes

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (lines, stripPrefix)
import qualified Data.Text.IO as Text (readFile)


type PortageR a = ReaderT PortagerConfig Identity a

runPortageR :: PortagerConfig -> ReaderT PortagerConfig Identity a -> a
runPortageR cfg r = runIdentity $ runReaderT r cfg

amd64 :: Arch
amd64 = Arch "amd64"

x86 :: Arch
x86 = Arch "x86"

parseWorldSets :: Text -> [WorldSet]
parseWorldSets = mapMaybe (Text.stripPrefix "@")  . Text.lines

readWorldSets :: FilePath -> IO [WorldSet]
readWorldSets = fmap parseWorldSets . Text.readFile

portager :: PortagerConfig -> [PortageR Set] -> IO ()
portager cfg ps = withOptions $ \opts -> do
  ws <- readWorldSets (_worldSets opts)
  runReaderT (writePortageSetConfigs ws $ map createPortageConfig $ runPortageR cfg $ sequence ps) opts
