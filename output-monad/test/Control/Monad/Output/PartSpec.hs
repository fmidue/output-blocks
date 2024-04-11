{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Output.PartSpec (spec) where


import Data.Map                  (Map, fromList, member)
import Generic.Random            (BaseCase(..), genericArbitraryU')
import Test.Hspec                (Spec, describe, it)
import Test.QuickCheck           (Arbitrary(..), Gen, chooseInt, forAll, vectorOf)
import Test.QuickCheck.Monadic   (assert, monadicIO, run)

import Control.Monad.Output      (Language)
import Control.Monad.Output.Part (OutputPart(..), getOutputParts, toOutputMonad)



instance BaseCase OutputPart where
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


instance Arbitrary OutputPart where
  arbitrary = genericArbitraryU'



spec :: Spec
spec = do
    describe "Language Maps" $
        it "always contain all languages for tests in this file" $
             forAll (arbitrary :: Gen (Map Language String)) $
              \langMap -> all (`member` langMap) ([minBound .. maxBound] :: [Language])

    describe "OutputPart" $
        it "converting to LangM and back yields original value" $
             forAll arbitrary $
              \outputParts -> monadicIO $ do
                new <- run $ getOutputParts $ toOutputMonad outputParts
                assert $ new == outputParts
