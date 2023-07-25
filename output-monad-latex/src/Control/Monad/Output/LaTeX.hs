{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Output.LaTeX (
  getLaTeX,
  toLaTeX,
  ) where

import qualified Data.Map as M

import Control.Monad.Output (
  Language,
  GenericLangM (LangM),
  GenericOut (Localised),
  GenericOutputMonad (..),
  GenericReportT (Report),
  ReportT,
  RunnableOutputMonad (..),
  abortWith,
  alignOutput,
  combineReports,
  combineTwoReports,
  format,
  getAllOuts,
  runLangMReport,
  toOutput,
  )

import Control.Monad.Writer (MonadWriter (tell))
import Data.Bifunctor (first)
import Data.Foldable (Foldable (foldl'))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Text.LaTeX.Base.Syntax (
  LaTeX (TeXComm, TeXEnv, TeXRaw),
  TeXArg (FixArg, OptArg),
  )

{-|
Retrieves the function to get LaTeX-Code when provided a 'Language'.
In contrast to 'execLangM' it throws a RuntimeException on abortion.
-}
toLaTeX :: Monad m => ReportT LaTeX m a -> m (Language -> LaTeX)
toLaTeX r = do
  os <- getAllOuts r
  return $ mconcat $ bs os
  where
    bs = fmap $ fromMaybe (error "was rejected") . toOutput

getLaTeX
  :: Monad m
  => ReportT LaTeX m a
  -> m (Either (Language -> LaTeX) (Language -> LaTeX))
getLaTeX r = do
  os <- getAllOuts r
  return $ foldl'
    (\xs x -> do
        xs' <- xs
        maybe (Left xs') (return . (xs' <>)) $ toOutput x)
    (Right mempty)
    os

par :: LaTeX
par = TeXRaw "\n\n"

newline :: LaTeX
newline = TeXRaw "\n"

transformFile :: String -> String
transformFile []     = []
transformFile "svg"  = "pdf"
transformFile (x:xs) = x:transformFile xs

instance GenericOutputMonad Language (ReportT LaTeX IO) where
  assertion p o = o *>
    if p
      then format . TeXEnv "quote" [] $ TeXRaw "Yes."
      else abortWith . TeXEnv "quote" [] $ TeXRaw "No."
  image f = format $ par
    <> TeXEnv "centering" [] (TeXComm "includegraphics"
    [OptArg $ TeXRaw "min width=0.4\\textwidth=0.6\\textwidth,min height=0.4\\textwidth=0.6\\textwidth,keepaspectratio,max width=0.9\\textwidth,max height=0.9\\textwidth", FixArg $ TeXRaw $ pack $ transformFile f])
    <> par
  images display f = format
    . (\x -> TeXComm "figureSeriesHere" [
         FixArg $ TeXRaw "", FixArg x])
    . M.foldrWithKey
    (\k x xs ->
       TeXComm "figureSeriesRow" [
         FixArg $ TeXComm "figureSeriesElement" [
           FixArg $ TeXRaw $ pack $ display k,
           FixArg $ TeXComm "includegraphics" [
               OptArg $ TeXRaw "min width=0.5\\textwidth,min height=0.4\\textwidth,max width=\\textwidth,max height=\\textwidth",
               FixArg $ TeXRaw $ pack $ transformFile $ f x
               ]
           ]
         ]
      <> xs
    )
    mempty
  paragraph = alignOutput ((<> par) . mconcat)
  text = format . TeXRaw . pack . (\xs -> ' ':xs ++ " ")
  refuse xs = xs *> abortWith mempty
  indent = alignOutput (TeXEnv "quote" [] . mconcat)
  enumerateM f =
      combineReports (TeXEnv "enumerate" [] . mconcat . concat)
      . map (uncurry processItem . first f)
    where
      processItem = combineTwoReports $ \x' y' ->
        TeXComm "item" [OptArg $ mconcat x', FixArg $ mconcat y']
        <> newline
  itemizeM = alignOutput
    (TeXEnv "itemize" [] . foldr (\x y -> TeXComm "item" [] <> x <> y) mempty) . sequenceA
  latex = format . TeXRaw . pack . ('$':) . (++ "$")
  code = format . TeXEnv "verbatim" [] . TeXRaw . pack
  translatedCode lm = LangM
    $ Report . tell . (:[]) . Localised
    $ TeXEnv "verbatim" [] . TeXRaw . pack . lm
  translated lm = LangM $ Report . tell . (:[]) . Localised $ TeXRaw . pack . lm

instance RunnableOutputMonad Language (ReportT LaTeX IO) where
  type RunMonad Language (ReportT LaTeX IO) = IO
  type Output Language (ReportT LaTeX IO) = LaTeX
  runLangM = runLangMReport mempty (<>)
