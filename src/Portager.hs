{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Portager (
    module Portager.DSL
  , PortageR
  , portager
  , runPortageR
) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Data.Maybe (mapMaybe)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text (lines, stripPrefix)
import qualified Data.Text.IO as Text (readFile)

import Portager.Options (Options(..), WorldSet, withOptions)
import Portager.Writes (createPortageSetConfig, writePortageSetConfigs)
import Portager.DSL

type PortageR a = ReaderT PortagerConfiguration Identity a

-- |Triggers Orphan Instances Warning. 
-- It seems that wrapping it in a newtype is not worth the trouble.
instance IsString a => IsString (PortageR a) where
  fromString = return . fromString

runPortageR :: PortagerConfiguration -> ReaderT PortagerConfiguration Identity a -> a
runPortageR cfg r = runIdentity $ runReaderT r cfg

parseWorldSets :: Text -> [WorldSet]
parseWorldSets = mapMaybe (Text.stripPrefix "@") . Text.lines

readWorldSets :: FilePath -> IO [WorldSet]
readWorldSets = fmap parseWorldSets . Text.readFile

portager :: PortagerConfiguration -> [PortageR PackageSet] -> IO ()
portager cfg ps = withOptions $ \opts -> do
  ws <- readWorldSets (_worldSets opts)
  runReaderT (writePortageSetConfigs ws $ map createPortageSetConfig $ runPortageR cfg $ sequence ps) opts
