{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Portager.Package where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, execStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, ReaderT, asks, runReaderT)

import Control.Lens (Lens', (<>~), (<>=), lens, set, view)

import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text (unwords)

type PortageT c a = StateT c (ReaderT PortagerConfig Identity) a

class ShowText a where
  showText :: a -> Text

instance ShowText Text where
  showText = id

instance ShowText a => ShowText [a] where
  showText = Text.unwords . map showText

type Name = Text

newtype Atom = Atom Name deriving (Eq, Ord, Show)

instance ShowText Atom where
  showText (Atom n) = n

instance IsString Atom where
  fromString = Atom . fromString

data Use = Use Bool Text deriving (Eq, Show)

instance Ord Use where
  Use _ t `compare` Use _ t' = t `compare` t'

instance ShowText Use where
  showText (Use True t) = t
  showText (Use False t) = "-" <> t

instance IsString Use where
  fromString s
    | "-" `isPrefixOf` s = Use False (fromString $ tail s)
    | otherwise = Use True (fromString s)

newtype Keyword = Keyword Text deriving (Eq, Show)

instance ShowText Keyword where
  showText (Keyword k) = k

newtype License = License Text deriving (Eq, Show)

instance IsString License where
  fromString = License . fromString

instance ShowText License where
  showText (License l) = l

newtype Arch = Arch Text deriving (Eq, Show)

arch :: Arch -> Text
arch (Arch t) = t

data Package = Package
  { _atom :: Atom
  , _configuration :: PackageConfiguration
  } deriving (Eq, Show)

instance IsString Package where
  fromString s = Package (fromString s) mempty 
data PackageConfiguration = PackageConfiguration
  { _useflags :: [Use]
  , _keywords :: [Keyword] 
  , _licenses :: [License]
  , _dependencies :: [Package]
  } deriving (Eq, Show)

instance IsString a => IsString (Reader PortagerConfig a) where
  fromString = return . fromString

keywordsL :: Lens' PackageConfiguration [Keyword]
keywordsL = lens _keywords (\cfg nks -> cfg { _keywords = nks })

licencesL :: Lens' PackageConfiguration [License]
licencesL = lens _licenses (\cfg nls -> cfg { _licenses = nls })

instance Monoid PackageConfiguration where
  mempty = PackageConfiguration mempty mempty mempty mempty
  PackageConfiguration u k d l `mappend` PackageConfiguration u' k' d' l' = 
    PackageConfiguration (u <> u') (k <> k') (d <> d') (l <> l')

license :: License -> PortageT PackageConfiguration ()
license l = licencesL <>= [l]

keywords :: [Keyword] -> PortageT PackageConfiguration ()
keywords ks = keywordsL <>= ks

class WithUseflags a where
  useL :: Lens' a [Use]
  use :: [Use] -> PortageT a ()
  uses :: [Use] -> PortageT a ()
  uses = use
  use us = useL <>= us

instance WithUseflags PackageConfiguration where
  useL = lens _useflags (\cfg nus -> cfg { _useflags = nus })

instance WithUseflags SetConfiguration where
  useL = lens _setUseflags (\cfg nus -> cfg { _setUseflags = nus })

class WithDependencies a where
  depL :: Lens' a [Package]
  dep :: [ReaderT PortagerConfig Identity Package] -> PortageT a ()
  -- just alias for nice alignments
  deps :: [ReaderT PortagerConfig Identity Package] -> PortageT a ()
  deps = dep
  dep ds = do
    ds' <- lift $ sequence ds
    depL <>= ds'

instance WithDependencies PackageConfiguration where
  depL = lens _dependencies (\cfg nds -> cfg { _dependencies = nds })

instance WithDependencies SetConfiguration where
  depL = lens _setDependencies (\cfg nds -> cfg { _setDependencies = nds })

data SetConfiguration = SetConfiguration
  { _setUseflags :: [Use]
  , _setPackages :: [Package]
  , _setDependencies :: [Package] 
  } deriving (Eq, Show)

setPackagesL :: Lens' SetConfiguration [Package]
setPackagesL = lens _setPackages (\cfg nps -> cfg { _setPackages = nps })

instance Monoid SetConfiguration where
  mempty = SetConfiguration mempty mempty mempty
  SetConfiguration u p d `mappend` SetConfiguration u' p' d' = 
    SetConfiguration (u <> u') (p <> p') (d <> d')

data Set = Set 
  { _setName :: Name
  , _setConfiguration:: SetConfiguration
  } deriving (Eq, Show) 

instance IsString Set where
  fromString s = Set (fromString s) mempty

pkgs :: [ReaderT PortagerConfig Identity Package] -> PortageT SetConfiguration ()
pkgs ps = do
  ps' <- lift $ sequence ps
  setPackagesL <>= ps'

unstable :: PortageT PackageConfiguration ()
unstable = do
  a <- lift $ asks _arch
  keywords [Keyword ("~" <> arch a)]

class (Monoid (Configuration w)) => With w where
  type Configuration w
  cfgL :: Lens' w (Configuration w)
  with :: w -> PortageT (Configuration w) () -> ReaderT PortagerConfig Identity w
  with w s = do
    cfg <- execStateT s mempty
    return $ set cfgL cfg w

instance With Package where
  type Configuration Package = PackageConfiguration
  cfgL = lens _configuration (\p nc -> p { _configuration = nc })

instance With Set where
  type Configuration Set = SetConfiguration
  cfgL = lens _setConfiguration (\s nc -> s { _setConfiguration = nc })

data PortagerConfig = PortagerConfig
  { _arch :: Arch
  } deriving (Eq, Show)
