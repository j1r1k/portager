module Portager.Flatten where

import Data.Foldable (foldr')
import Data.Semigroup (First(..), Semigroup(..), (<>), sconcat)
import Data.List (groupBy, union)
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (nonEmpty)

import Portager.Package

data FlatPackage = FlatPackage 
  { _fpAtom :: Atom
  , _fpUseflags :: [Use]
  , _fpKeywords :: [Keyword]
  , _fpLicenses :: [License]
  } deriving (Eq, Show)

instance Ord FlatPackage where
  fp `compare` fp' = _fpAtom fp `compare` _fpAtom fp'

instance Semigroup FlatPackage where
  (FlatPackage a u k l) <> (FlatPackage a' u' k' l') = 
    FlatPackage a (u `unionUseflags` u') (k `union` k') (l `union` l')

safeSconcat :: Semigroup a => [a] -> Maybe a
safeSconcat = fmap sconcat . nonEmpty

unionBy :: (Semigroup a, Eq b) => (a -> b) -> [a] -> [a] -> [a]
unionBy get as as' = mapMaybe safeSconcat $ groupBy (\a b -> get a == get b) $ as ++ as'

unionUseflags :: [Use] -> [Use] -> [Use]
unionUseflags as bs = getFirst <$> unionBy (flag . getFirst) (First <$> as) (First <$> bs)
  where flag (Use _ t) = t

flattenPackage :: [Use] -> Package -> FlatPackage
flattenPackage global pkg = 
  let cfg = _configuration pkg 
   in FlatPackage (_atom pkg) (_useflags cfg `unionUseflags` global) (_keywords cfg) (_licenses cfg)

unionFlatPackages :: [FlatPackage] -> [FlatPackage] -> [FlatPackage]
unionFlatPackages = unionBy _fpAtom

flatten :: SetConfiguration -> Package -> [FlatPackage]
flatten cfg pkg = 
  let fp = flattenPackage (_setUseflags cfg) pkg 
      fdeps = (_dependencies . _configuration) pkg >>= flatten cfg
   in [fp] `unionFlatPackages` fdeps

flattenPackages :: SetConfiguration -> [Package] -> [FlatPackage]
flattenPackages cfg = foldr' step mempty
  where step pkg flats = flats `unionFlatPackages` flatten cfg pkg

flattenSet :: Set -> [FlatPackage]
flattenSet s = 
  let cfg = _setConfiguration s
  in flattenPackages cfg (_setPackages cfg) `unionFlatPackages` flattenPackages cfg (_setDependencies cfg)
