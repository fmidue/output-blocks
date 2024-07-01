{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.OutputCapable.Blocks.LaTeXSpec where

import Control.OutputCapable.Blocks (
  GenericLangM (unLangM),
  GenericOutputCapable (..),
  LangM,
  Language (..),
  OutputCapable,
  ReportT,
  english,
  german,
  translate,
  )
import Control.OutputCapable.Blocks.Generic (
  evalLangM,
  execLangM,
  )
import Control.OutputCapable.Blocks.LaTeX (toLaTeX)
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
    it "should throw an exception on evaluating a refusion using toLaTeX" $
      do o <- render . ($ English) <$> toLaTeX (unLangM abort)
         print o
      `shouldThrow` anyErrorCall
    it "should abort at refusion (and not earlier)" $
      render . ($ English) <$> execLangM abort
      `shouldReturn` " a \n\n True \\begin{quote}Yes.\\end{quote}\n\n False \\begin{quote}No.\\end{quote}\n\n"
    it "should abort at refusion (and not earlier) in enumeration" $
      render . ($ English) <$> execLangM abort2
      `shouldReturn` "\\begin{enumerate}\\item[ 1 ]{ a }\n\\item[ NO! ]{}\n\\end{enumerate}\n\n"
    it "abort should evaluate to Nothing" $
      evalLangM abort `shouldReturn` Nothing
  where
    language = paragraph $ translate $ do
      english "English"
      german "deutsch"
    paragraphs :: OutputCapable m => LangM m
    paragraphs = do
      paragraph $ text "a"
      paragraph $ text "b"
      pure ()
    abort = do
      paragraph $ text "a"
      paragraph $ assertion True $ text "True"
      paragraph $ assertion False $ text "False"
      paragraph $ refuse $ text "aborted"
      translation
      pure ()
    translation =
      paragraph $ translate $ do
        english "foo"
        german "bar"
    abort2 = do
      paragraph $ enumerateM (\x -> if x == "2" then refuse $ text "NO!" else text x) [
        ("1", text "a"),
        ("2", text "b"),
        ("3", refuse $ text "not c"),
        ("4", text "d")
        ]
      translation
      pure ()
