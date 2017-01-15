{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, execStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, ReaderT, asks, runReaderT)

import Control.Lens (Lens', (<>=), set, lens)

import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

type PortagerT c a = StateT c (ReaderT PortagerConfig Identity) a

type Name = Text

newtype Atom = Atom Name deriving (Eq, Show)

instance IsString Atom where
  fromString = Atom . fromString

data Use = Use Bool Text deriving (Eq, Show)

instance IsString Use where
  fromString s
    | "-" `isPrefixOf` s = Use False (fromString $ tail s)
    | otherwise = Use True (fromString s)

newtype Keyword = Keyword Text deriving (Eq, Show)

newtype License = License Text deriving (Eq, Show)

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

keywordsL :: Lens' PackageConfiguration [Keyword]
keywordsL = lens _keywords (\cfg nks -> cfg { _keywords = nks })

licencesL :: Lens' PackageConfiguration [License]
licencesL = lens _licenses (\cfg nls -> cfg { _licenses = nls })

instance Monoid PackageConfiguration where
  mempty = PackageConfiguration mempty mempty mempty mempty
  PackageConfiguration u k d l `mappend` PackageConfiguration u' k' d' l' = 
    PackageConfiguration (u <> u') (k <> k') (d <> d') (l <> l')

license :: License -> PortagerT PackageConfiguration ()
license l = licencesL <>= [l]

keywords :: [Keyword] -> PortagerT PackageConfiguration ()
keywords ks = keywordsL <>= ks

class WithUseflags a where
  useL :: Lens' a [Use]
  use :: [Use] -> PortagerT a ()
  use us = useL <>= us

instance WithUseflags PackageConfiguration where
  useL = lens _useflags (\cfg nus -> cfg { _useflags = nus })

instance WithUseflags SetConfiguration where
  useL = lens _setUseflags (\cfg nus -> cfg { _setUseflags = nus })

class WithDependencies a where
  depL :: Lens' a [Package]
  dep :: [ReaderT PortagerConfig Identity Package] -> PortagerT a ()
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

instance IsString (Reader PortagerConfig Set) where
  fromString = return . fromString

pkgs :: [ReaderT PortagerConfig Identity Package] -> PortagerT SetConfiguration ()
pkgs ps = do
  ps' <- lift $ sequence ps
  setPackagesL <>= ps'

unstable :: PortagerT PackageConfiguration ()
unstable = do
  a <- lift $ asks _arch
  keywords [Keyword ("~" <> arch a)]

class Monoid c => With w c where
  cfgL :: Lens' w c
  with :: w -> PortagerT c () -> ReaderT PortagerConfig Identity w
  with w s = do
    cfg <- execStateT s mempty
    return $ set cfgL cfg w

instance With Package PackageConfiguration where
  cfgL = lens _configuration (\p nc -> p { _configuration = nc })

instance With Set SetConfiguration where
  cfgL = lens _setConfiguration (\s nc -> s { _setConfiguration = nc })

data PortagerConfig = PortagerConfig
  { _arch :: Arch
  } deriving (Eq, Show)

withConfig :: Arch -> ReaderT PortagerConfig Identity a -> a
withConfig a r = runIdentity $ runReaderT r (PortagerConfig a)

amd64 :: Arch
amd64 = Arch "amd64"

x86 :: Arch
x86 = Arch "x86"

testSet :: ReaderT PortagerConfig Identity Set
testSet = "eurus" `with`
            pkgs [
              "sys-firmware/iwl7260-ucode" `with` do
                  unstable
                  use [ "bluetooth", "-test" ]
                  dep [ "sys-firmware/iwl3160-7260-bt-ucode" `with` unstable ]
            ]