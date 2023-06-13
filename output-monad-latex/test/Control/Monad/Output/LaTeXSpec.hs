{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Output.LaTeXSpec where

import Control.Monad.Output (LangM' (..), Language (..), OutputMonad (..), english, german, translate)
import Control.Monad.Output.LaTeX (toLaTeX)
import Text.LaTeX.Base.Syntax (LaTeX (..))

import Test.Hspec

spec :: Spec
spec =
  context "toLaTeX" $ do
    it "retrieves English version" $
      toLaTeX English (($ English) <$> unLangM language) `shouldReturn` TeXRaw "English"
    it "retrieves German version" $
      toLaTeX German (unLangM language) `shouldReturn` TeXRaw "deutsch"
  where
    language = paragraph $ translate $ do
      english "English"
      german "deutsch"
