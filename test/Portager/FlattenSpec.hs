{-# LANGUAGE OverloadedStrings #-}
module Portager.FlattenSpec where

import qualified Data.Set as Set (fromList)

import Test.Hspec
import Test.QuickCheck

import Portager.DSL
import Portager.Flatten

import Portager.DSLSpec

flipUse :: Use -> Use
flipUse (Use b t) = Use (not b) t

spec :: IO ()
spec = hspec $ do
  describe "Portager.Flatten" $ do
    describe "flattenPackage" $ do
      it "converts 'Package' to 'FlatPackage' as is, discarding dependencies, when no globals are defined" $
        property $ \atom useflags keywords licenses dependencies ->
          let pkg = Package atom $ PackageConfiguration useflags keywords licenses dependencies
              expected = FlatPackage atom (Set.fromList useflags) (Set.fromList keywords) (Set.fromList licenses)
           in flattenPackage mempty pkg == expected
      it "convers 'Package' to 'FlatPackage' as is, discarding dependencies, prefering locals before globals" $
        property $ \atom useflags keywords licenses dependencies ->
          let pkg = Package atom $ PackageConfiguration useflags keywords licenses dependencies
              globals = Set.fromList $ map flipUse useflags
              expected = FlatPackage atom (Set.fromList useflags) (Set.fromList keywords) (Set.fromList licenses)
           in flattenPackage globals pkg == expected 


