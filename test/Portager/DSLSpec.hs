module Portager.DSLSpec where

import Control.Monad.State (execState)

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text (pack)

import Test.Hspec
import Test.QuickCheck

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
    arbitrary = resize 3 genPackageConfiguration

instance Arbitrary Package where
    arbitrary = resize 3 genPackage

instance Arbitrary SetConfiguration where
    arbitrary = SetConfiguration <$> resize 5 arbitrary <*> resize 5 arbitrary <*> resize 5 arbitrary

instance Arbitrary PackageSet where
    arbitrary = PackageSet <$> arbitrary <*> arbitrary

spec :: IO ()
spec = hspec $ do
  describe "Protager.DSL" $ do

    describe "keywords" $ do
      it "appending empty keywords does not change configuration" $
        property $ \cfg -> execState (keywords mempty) cfg == cfg

      it "appends keyword to keywords" $ do
        property $ \kws cfg -> execState (keywords kws) cfg == cfg { _keywords = (_keywords cfg) <> kws }

    describe "license" $ do
      it "appends license to licenses" $ do
        property $ \l cfg -> execState (license l) cfg == cfg { _licenses = (_licenses cfg) <> [l] }
