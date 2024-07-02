{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.OutputCapable.Blocks.Generic.TypeSpec (spec) where


import Data.Map                  (Map, fromList, member)
import Generic.Random            (BaseCase (..), W, (%), genericArbitrary')
import Test.Hspec                (Spec, describe, it)
import Test.QuickCheck           (Arbitrary(..), Gen, chooseInt, forAll, vectorOf)
import Test.QuickCheck.Monadic   (assert, monadicIO, run)

import Control.OutputCapable.Blocks     (Language (English))
import Control.OutputCapable.Blocks.Generic.Type (
  GenericOutput (..),
  getOutputSequence,
  toOutputCapable,
  )

instance BaseCase (GenericOutput Language a) where
  baseCase = do
    which <- chooseInt (1,5)
    case which of
      1 -> Latex <$> arbitrary
      2 -> Translated <$> arbitrary
      3 -> Code <$> arbitrary
      4 -> Image <$> arbitrary
      5 -> Images <$> arbitrary
      _ -> error "impossible constructor"


instance {-# Overlapping #-} Arbitrary (Map Language String) where
  arbitrary = do
    let langs = [minBound .. maxBound]
    texts <- vectorOf (length langs) arbitrary
    pure $ fromList $ zip langs texts


instance Arbitrary (GenericOutput Language ()) where
  arbitrary = genericArbitrary'
    $ 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % (0 :: W "Special") % ()



spec :: Spec
spec = do
    describe "Language Maps" $
        it "always contain all languages for tests in this file" $
             forAll (arbitrary :: Gen (Map Language String)) $
              \langMap -> all (`member` langMap) ([minBound .. maxBound] :: [Language])

    describe "GenericOutput (without Special)" $
        it "converting to LangM and back yields original value" $
             forAll arbitrary $
              \outputSequence -> monadicIO $ do
                new <- run $ getOutputSequence English
                  $ toOutputCapable pure outputSequence
                assert $ new == outputSequence
