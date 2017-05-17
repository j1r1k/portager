{-# LANGUAGE OverloadedStrings #-}
module Portager.WritesSpec where

import qualified Data.Set as Set (fromList)

import Portager
import Portager.DSL
import Portager.Flatten
import Portager.Writes

import Test.Hspec
import Test.QuickCheck

import Portager.DSLSpec

spec :: IO ()
spec = hspec $ do
  let emptyFlatPackage = FlatPackage (Atom "games-fps/quake3-demo") mempty mempty mempty

  describe "toPackageUseRecord" $ do
    it "does not create a text record for FlatPackage without useflags" $
      toPackageUseRecord emptyFlatPackage `shouldBe` Nothing

    it "creates a text record for package.use file for FlatPackage with alphabeticaly sorted useflags" $
      let fp = FlatPackage (Atom "dev-db/sqlite") (Set.fromList ["secure-delete", "-debug", "readline"]) mempty mempty
       in toPackageUseRecord fp `shouldBe` Just "dev-db/sqlite -debug readline secure-delete"

  describe "toPackageAcceptKeywordRecord" $ do
    it "does not create a text record for FlatPackage without keywords" $
      toPackageAcceptKeywordRecord emptyFlatPackage `shouldBe` Nothing

    it "creates a text record for a package.accept_keywords file for FlatPackage with alphabeticaly sorted keywords" $
       let fp = FlatPackage (Atom "games-fps/quake3-demo") mempty (Set.fromList ["~amd64", "~x86"]) mempty
        in toPackageAcceptKeywordRecord fp `shouldBe` Just "games-fps/quake3-demo ~amd64 ~x86"

  describe "toPackageLicenseRecord" $ do
    it "does not create a text record for FlatPackage without licenses" $
      toPackageLicenseRecord emptyFlatPackage `shouldBe` Nothing

    it "creates a text record for a package.license file for FlatPackage with alphabeticaly sorted licenses" $
      let fp = FlatPackage (Atom "net-im/gitter-bin") mempty mempty (Set.fromList ["MIT", "no-source-code"])
       in toPackageLicenseRecord fp `shouldBe` Just "net-im/gitter-bin MIT no-source-code"

  describe "toPortageSet" $
    it "creates a list of Atom names for a portage set file" $
      let setPackages = ["games-fps/quake3-demo", "dev-db/sqlite"] :: [Package]
          cfg = SetConfiguration mempty setPackages mempty
       in toPortageSet cfg `shouldBe` ["games-fps/quake3-demo", "dev-db/sqlite"]

