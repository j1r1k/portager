-- |
-- Module      :  Portager.DSL
-- Copyright   :  (C) 2017 Jiri Marsicek
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jiri Marsicek <jiri.marsicek@gmail.com>
--
-- This module defines portage configuration DSL language.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Portager.DSL
    ( Arch(..)
    , amd64
    , x86
    , ShowText(..)
    , Name
    , Atom(..)
    , Use(..)
    , Keyword(..)
    , License(..)
    , PackageConfiguration(..)
    , keywordsL
    , keywords
    , unstable
    , licencesL
    , license
    , Package(..)
    , SetConfiguration(..)
    , setPackagesL
    , pkgs
    , PackageSet(..)
    , WithUseflags(..)
    , WithDependencies(..)
    , With(..)
    , PortagerConfiguration(..)
    ) where

import Control.Monad.Identity (Identity)
import Control.Monad.State (MonadState, StateT, execStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks)

import Control.Lens (Lens', (<>=), lens, set)

import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text (unwords)

-- |An Architecture Keyword
newtype Arch = Arch { arch :: Text } deriving (Eq, Show)

instance IsString Arch where
  fromString = Arch . fromString

amd64 :: Arch
amd64 = Arch "amd64"

x86 :: Arch
x86 = Arch "x86"

-- |Global configuration
data PortagerConfiguration = PortagerConfiguration { _arch :: Arch } deriving (Eq, Show)

type PortageT c a = StateT c (ReaderT PortagerConfiguration Identity) a

class ShowText a where
  showText :: a -> Text

instance ShowText Text where
  showText = id

instance ShowText a => ShowText [a] where
  showText = Text.unwords . map showText


type Name = Text

-- |Portage Atom
newtype Atom = Atom Name deriving (Eq, Ord, Show)

instance ShowText Atom where
  showText (Atom n) = n

instance IsString Atom where
  fromString = Atom . fromString


data Use = Use Bool Text deriving (Eq, Show)

instance Ord Use where
  Use _ t `compare` Use _ t' = t `compare` t'

instance IsString Use where
  fromString s
    | "-" `isPrefixOf` s = Use False (fromString $ tail s)
    | otherwise = Use True (fromString s)

instance ShowText Use where
  showText (Use True t) = t
  showText (Use False t) = "-" <> t

-- |Portage Keyword
newtype Keyword = Keyword Text deriving (Eq, Ord, Show)

instance IsString Keyword where
  fromString = Keyword . fromString

instance ShowText Keyword where
  showText (Keyword k) = k

-- |Portage License
newtype License = License Text deriving (Eq, Ord, Show)

instance IsString License where
  fromString = License . fromString

instance ShowText License where
  showText (License l) = l

data PackageConfiguration = PackageConfiguration
  { _useflags :: [Use]
  , _keywords :: [Keyword]
  , _licenses :: [License]
  , _dependencies :: [Package]
  } deriving (Eq, Show)

instance Monoid PackageConfiguration where
  mempty = PackageConfiguration mempty mempty mempty mempty
  PackageConfiguration u k d l `mappend` PackageConfiguration u' k' d' l' =
    PackageConfiguration (u <> u') (k <> k') (d <> d') (l <> l')


keywordsL :: Lens' PackageConfiguration [Keyword]
keywordsL = lens _keywords (\cfg nks -> cfg { _keywords = nks })

-- |Appends 'Keyword's to a 'PackageConfiguration'.
keywords :: MonadState PackageConfiguration m => [Keyword] -> m ()
keywords ks = keywordsL <>= ks

-- |Appends unstable keyword for globally configured architecture to a 'PackageConfiguration'.
unstable :: PortageT PackageConfiguration ()
unstable = do
  a <- lift $ asks _arch
  keywords [ Keyword ("~" <> arch a) ]

licencesL :: Lens' PackageConfiguration [License]
licencesL = lens _licenses (\cfg nls -> cfg { _licenses = nls })

-- |Appends 'License's to a 'PackageConfiguration'
license :: MonadState PackageConfiguration m => License -> m ()
license l = licencesL <>= [l]


data Package = Package
  { _atom :: Atom
  , _configuration :: PackageConfiguration
  } deriving (Eq, Show)

instance Ord Package where
  a `compare` b = _atom a `compare` _atom b

instance IsString Package where
  fromString s = Package (fromString s) mempty


class WithUseflags a where
  useL :: Lens' a [Use]
  -- |Appends 'Use's to an encapsulated configuration.
  use :: [Use] -> PortageT a ()
  -- an alias for nice alignments
  uses :: [Use] -> PortageT a ()
  uses = use
  use us = useL <>= us

instance WithUseflags PackageConfiguration where
  useL = lens _useflags (\cfg nus -> cfg { _useflags = nus })

instance WithUseflags SetConfiguration where
  useL = lens _setUseflags (\cfg nus -> cfg { _setUseflags = nus })


class WithDependencies a where
  depL :: Lens' a [Package]
  -- |Appends 'Package's as dependencies to an encapsulated configuration.
  dep :: [ReaderT PortagerConfiguration Identity Package] -> PortageT a ()
  -- an alias for nice alignments
  deps :: [ReaderT PortagerConfiguration Identity Package] -> PortageT a ()
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

instance Monoid SetConfiguration where
  mempty = SetConfiguration mempty mempty mempty
  SetConfiguration u p d `mappend` SetConfiguration u' p' d' =
    SetConfiguration (u <> u') (p <> p') (d <> d')

setPackagesL :: Lens' SetConfiguration [Package]
setPackagesL = lens _setPackages (\cfg nps -> cfg { _setPackages = nps })

-- |Appends 'Package's to a 'SetConfiguration' as explicit dependencies (listed in set file)
pkgs :: [ReaderT PortagerConfiguration Identity Package] -> PortageT SetConfiguration ()
pkgs ps = do
  ps' <- lift $ sequence ps
  setPackagesL <>= ps'

data PackageSet = PackageSet
  { _setName :: Name
  , _setConfiguration :: SetConfiguration
  } deriving (Eq, Show)

instance IsString PackageSet where
  fromString s = PackageSet (fromString s) mempty

-- |A class for modifications to an encapsulated configuration of 'w'
class (Monoid (Configuration w)) => With w where
  type Configuration w
  configurationL :: Lens' w (Configuration w)
  with :: w -> PortageT (Configuration w) () -> ReaderT PortagerConfiguration Identity w
  with w s = do
    cfg <- execStateT s mempty
    pure $ set configurationL cfg w

instance With Package where
  type Configuration Package = PackageConfiguration
  configurationL = lens _configuration (\p nc -> p { _configuration = nc })

instance With PackageSet where
  type Configuration PackageSet = SetConfiguration
  configurationL = lens _setConfiguration (\s nc -> s { _setConfiguration = nc })
