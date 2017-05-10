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
import Portager.Writes (createPortageConfig, writePortageSetConfigs)
import Portager.DSL

newtype PortageR a = PortageR (ReaderT PortagerConfiguration Identity a)

instance Functor PortageR where
  fmap f (PortageR r) = PortageR (fmap f r)

instance Applicative PortageR where
  pure = PortageR . pure
  (PortageR f) <*> (PortageR a) = PortageR (f <*> a)

instance Monad PortageR where
  (PortageR a) >>= f = PortageR (a >>= unwrapPortageR . f)
    where unwrapPortageR (PortageR r) = r

instance IsString a => IsString (PortageR a) where
  fromString = pure . fromString

runPortageR :: PortagerConfiguration -> PortageR a -> a
runPortageR cfg (PortageR r) = runIdentity $ runReaderT r cfg

parseWorldSets :: Text -> [WorldSet]
parseWorldSets = mapMaybe (Text.stripPrefix "@") . Text.lines

readWorldSets :: FilePath -> IO [WorldSet]
readWorldSets = fmap parseWorldSets . Text.readFile

portager :: PortagerConfiguration -> [PortageR Set] -> IO ()
portager cfg ps = withOptions $ \opts -> do
  ws <- readWorldSets (_worldSets opts)
  runReaderT (writePortageSetConfigs ws $ map createPortageConfig $ runPortageR cfg $ sequence ps) opts
