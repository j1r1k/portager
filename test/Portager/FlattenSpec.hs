{-# LANGUAGE OverloadedStrings #-}
module Portager.FlattenSpec where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, singleton)

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

      it "converts 'Package' to 'FlatPackage' as is, discarding dependencies, prefering locals before globals" $
        property $ \atom useflags keywords licenses dependencies ->
          let pkg = Package atom $ PackageConfiguration useflags keywords licenses dependencies
              globals = Set.fromList $ map flipUse useflags
              expected = FlatPackage atom (Set.fromList useflags) (Set.fromList keywords) (Set.fromList licenses)
           in flattenPackage globals pkg == expected 

    describe "mergeLists" $ do
      it "preserves ordering" $
        property $ \xs ys -> mergeLists (sort xs) (sort ys) == sort (xs ++ ys :: [Int])

    describe "merge" $ do
      it "merging with right empty set does not change input set if equality is determined by the element itself" $
        property $ \xs -> merge id xs Set.empty == (xs :: Set [Int])

      it "merging with left empty set does not change input set if equality is determined by the element itself" $
        property $ \xs -> merge id Set.empty xs == (xs :: Set [Int])

      it "merges two sets using sconcat for elements equal by a result of given function" $
        let merged = merge length (Set.fromList [[], [1], [0], [10, 11]]) 
                                  (Set.fromList [[2], [20, 22]]) 
            expected = Set.fromList [[], [0, 1, 2], [10, 11, 20, 22]]
         in merged `shouldBe` expected
