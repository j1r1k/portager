-- |
-- Module      :  Portager.Flatten
-- Copyright   :  (C) 2017 Jiri Marsicek
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jiri Marsicek <jiri.marsicek@gmail.com>
--
-- This module provides functionality for converting 'Package's to 'FlatPackage's.
--
module Portager.Flatten where

import Data.Foldable (foldr')
import qualified Data.List as List (groupBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import qualified Data.Set as Set (fromList, singleton, toAscList, union)

import Portager.DSL

-- |Flat representation of 'Package' without nested dependencies.
data FlatPackage = FlatPackage
  { _fpAtom :: Atom
  , _fpUseflags :: Set Use
  , _fpKeywords :: Set Keyword
  , _fpLicenses :: Set License
  } deriving (Eq, Show)

instance Ord FlatPackage where
  fp `compare` fp' = _fpAtom fp `compare` _fpAtom fp'

instance Semigroup FlatPackage where
  FlatPackage atom us ks ls <> FlatPackage _ us' ks' ls' = 
    FlatPackage atom (us `Set.union` us') (ks `Set.union` ks') (ls `Set.union` ls')

-- |Merges two lists together, if both lists are sorted in ascending order, the result will be also sorted
-- in ascending order.
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] bs = bs
mergeLists as [] = as
mergeLists (a : as) (b : bs)
  | a <= b = a : mergeLists as (b : bs)
  | otherwise = b : mergeLists (a : as) bs

-- |Merges two sets together, performing 'sconcat' on elements that are determined to be equal by result
-- of a function given in argument.
merge :: (Ord a, Semigroup a, Eq b) => (a -> b) -> Set a -> Set a -> Set a
merge get lefts rights =
  Set.fromList $
    map sconcat $
    mapMaybe nonEmpty $
    List.groupBy (\a b -> get a == get b) $
    mergeLists (Set.toAscList lefts) (Set.toAscList rights)

-- |Performs 'merge' of two sets of 'FlatPackage's where equality is determined by 'Atom'.
-- See 'merge'.
mergePackages :: Set FlatPackage -> Set FlatPackage -> Set FlatPackage
mergePackages = merge _fpAtom

-- |Converts a 'Package' to a 'FlatPackage' with globals 'Use' flags applied.
-- Package 'Use' flags take precedence before globals 'Use' flags.
flattenPackage :: Set Use -> Package -> FlatPackage
flattenPackage globals pkg =
  let pkgcfg = _configuration pkg
      ufs = Set.fromList $ _useflags pkgcfg
      kws = Set.fromList $ _keywords pkgcfg
      lcs = Set.fromList $ _licenses pkgcfg
   in FlatPackage (_atom pkg) (ufs `Set.union` globals) kws lcs

-- |Converts a 'Package' to a list of 'FlatPackages' with set 'Use' flags applied.
flatten :: Set Use -> Package -> Set FlatPackage
flatten globals pkg =
  let fp = Set.singleton $ flattenPackage globals pkg
      fdeps = flattenPackages globals $ _dependencies $ _configuration pkg
   in mergePackages fp fdeps

-- |Convert a list of 'Package's to a list of 'FlatPackages' with set 'Use' flags applied.
-- See 'flatten' for details on how a single 'Package' is converted to a list of 'FlatPackage's.
flattenPackages :: Set Use -> [Package] -> Set FlatPackage
flattenPackages globals = foldr' step mempty
  where step pkg flats = mergePackages flats $ flatten globals pkg

-- |Converts a 'PackageSet' to a list of 'FlatPackage's.
-- See 'flattenPackages' for details on how list of 'Package's is converted to a list of 'FlatPackage's.
flattenSet :: PackageSet -> Set FlatPackage
flattenSet s =
  let cfg = _setConfiguration s
      globals = Set.fromList $ _setUseflags cfg
      setPkgs = flattenPackages globals $ _setPackages cfg
      setDeps = flattenPackages globals $ _setDependencies cfg
   in mergePackages setPkgs setDeps
