{-# LANGUAGE OverloadedStrings #-}
module Portager.DSLSpec where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (execState, execStateT)

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text (pack)

import Test.Hspec
import Test.QuickCheck

import Portager
import Portager.DSL

instance Arbitrary Text where
    arbitrary = Text.pack <$> (resize 5 $ arbitrary)

instance Arbitrary Arch where
    arbitrary = Arch <$> arbitrary

instance Arbitrary Atom where
    arbitrary = Atom <$> arbitrary

instance Arbitrary Use where
    arbitrary = Use <$> arbitrary <*> arbitrary

instance Arbitrary Keyword where
    arbitrary = Keyword <$> arbitrary

instance Arbitrary License where
    arbitrary = License <$> arbitrary

genPackage :: Gen Package
genPackage = sized (\n -> Package <$> arbitrary <*> resize n arbitrary)

genPackageConfiguration :: Gen PackageConfiguration
genPackageConfiguration = sized (\n -> PackageConfiguration <$> resize 5 arbitrary <*> resize 5 arbitrary <*> resize 5 arbitrary <*> deps n)
      where deps 0 = return []
            deps n = resize (n - 1) $ listOf genPackage


instance Arbitrary PackageConfiguration where
    arbitrary = resize 2 genPackageConfiguration

instance Arbitrary Package where
    arbitrary = resize 2 genPackage

instance Arbitrary SetConfiguration where
    arbitrary = SetConfiguration <$> resize 5 arbitrary <*> resize 5 arbitrary <*> resize 5 arbitrary

instance Arbitrary PackageSet where
    arbitrary = PackageSet <$> arbitrary <*> arbitrary

spec :: IO ()
spec = hspec $ do
  describe "Protager.DSL" $ do
    let configuration = PortagerConfiguration amd64

    describe "keywords" $ do
      it "appending empty keywords does not change configuration" $
        property $ \cfg -> execState (keywords mempty) cfg == cfg

      it "appends keyword to keywords" $
        property $ \kws cfg -> execState (keywords kws) cfg == cfg { _keywords = (_keywords cfg) <> kws }

    describe "license" $
      it "appends license to licenses" $ do
        property $ \l cfg -> execState (license l) cfg == cfg { _licenses = (_licenses cfg) <> [l] }

    describe "use" $ do
      it "appends useflag to package useflags" $
        property $ \uf cfg -> (runPortageR configuration (execStateT (use [uf]) (cfg :: PackageConfiguration))) == cfg { _useflags = (_useflags cfg) <> [uf] }

      it "appends useflag to set useflags" $
        property $ \uf cfg -> (runPortageR configuration (execStateT (use [uf]) (cfg :: SetConfiguration))) == cfg { _setUseflags = (_setUseflags cfg) <> [uf] }

    describe "dep" $ do
      it "appends package to package dependencies" $
        property $ \pkg cfg -> (runPortageR configuration (execStateT (dep [pure pkg]) (cfg :: PackageConfiguration))) == cfg { _dependencies = (_dependencies cfg) <> [pkg] }

      it "appends package to set dependencies" $
        property $ \pkg cfg -> (runPortageR configuration (execStateT (dep [pure pkg]) (cfg :: SetConfiguration))) == cfg { _setDependencies = (_setDependencies cfg) <> [pkg] }
    
    describe "unstable" $ do
      it "appends arch unstable keyword to keywords" $
        let kwText = arch $ _arch configuration 
         in (runPortageR configuration (execStateT unstable mempty)) == mempty { _keywords = [Keyword $ "~" <> kwText] }

    describe "pkgs" $ do
      it "appends package to set configuration" $ do
        property $ \pkg cfg -> (runPortageR configuration (execStateT (pkgs [pure pkg]) cfg)) == cfg { _setPackages = (_setPackages cfg) <> [pkg] }

    describe "IsString" $ do

      describe "Arch" $
        it "should parse Arch from string" $
          "amd64" `shouldBe` Arch "amd64"

      describe "Atom" $
        it "should parse Atom from string" $
          "games-fps/quake3-demo" `shouldBe` Atom "games-fps/quake3-demo"
      
      describe "Use" $ do
        it "should parse enabled Use from string" $
          "debug" `shouldBe` Use True "debug"

        it "should parse disabled Use from string" $
          "-debug" `shouldBe` Use False "debug"

      describe "Keyword" $ 
        it "should parse Keyword from string" $
          "~amd64" `shouldBe` Keyword "~amd64"

      describe "License" $
        it "should parse License from string" $
          "Oracle-BCLA-JavaSE" `shouldBe` License "Oracle-BCLA-JavaSE"

      describe "Package" $
        it "should create Package with empty configuration" $
          "games-fps/quake3-demo" `shouldBe` Package (Atom "games-fps/quake3-demo") mempty

      describe "PackageSet" $
        it "should create PackageSet with empty configuration" $
          "games" `shouldBe` PackageSet "games" mempty
