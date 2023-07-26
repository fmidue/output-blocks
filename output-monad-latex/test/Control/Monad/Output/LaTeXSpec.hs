{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Output.LaTeXSpec where

import Control.Monad.Output (
  GenericLangM (unLangM),
  GenericOutputMonad (..),
  LangM,
  Language (..),
  OutputMonad,
  ReportT,
  english,
  german,
  translate,
  )
import Control.Monad.Output.Generic (
  evalLangM,
  execLangM,
  )
import Control.Monad.Output.LaTeX (toLaTeX)
import Text.LaTeX (LaTeX)
import Text.LaTeX.Base.Render (render)
import Test.Hspec

spec :: Spec
spec =
  context "toLaTeX" $ do
    it "retrieves English version" $
      render . ($ English) <$> execLangM language
      `shouldReturn` "English\n\n"
    it "retrieves German version" $
      render . ($ German) <$> toLaTeX (unLangM language)
      `shouldReturn` "deutsch\n\n"
    it "retrieves paragraphs in order" $
      render . ($ German) <$> execLangM
        (paragraphs *> refuse (text "Abort!"):: LangM (ReportT LaTeX IO))
      `shouldReturn` " a \n\n b \n\n Abort! "
    it "retrieves paragraphs in order" $
      render . ($ German) <$> toLaTeX (unLangM paragraphs)
      `shouldReturn` " a \n\n b \n\n"
    it "output is the same for different languages if no translation is involved" $ do
      tex <- toLaTeX (unLangM paragraphs)
      return (tex German == tex English) `shouldReturn` True
    it "output should evaluate to Just ()" $ do
      evalLangM language `shouldReturn` Just ()
    it "should   throw an exception on evaluating a refusion using toLaTeX" $
      do o <- render . ($ English) <$> toLaTeX (unLangM abort)
         print o
      `shouldThrow` anyErrorCall
    it "should abort at refusion (and not earlier)" $
      render . ($ English) <$> execLangM abort
      `shouldReturn` " a \n\n True \\begin{quote}Yes.\\end{quote}\n\n"
    it "abort should evaluate to Nothing" $
      evalLangM abort `shouldReturn` Nothing
  where
    language = paragraph $ translate $ do
      english "English"
      german "deutsch"
    paragraphs :: OutputMonad m => LangM m
    paragraphs = do
      paragraph $ text "a"
      paragraph $ text "b"
      pure ()
    abort = do
      paragraph $ text "a"
      paragraph $ assertion True $ text "True"
      paragraph $ assertion False $ text "False"
      paragraph $ refuse $ text "aborted"
      paragraph $ translate $ do
        english "foo"
        german "bar"
      pure ()
