{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Output.LaTeX (
  getLaTeX,
  toLaTeX,
  ) where

import qualified Data.Map as M

import Control.Monad.Output (
  Language,
  LangM' (LangM),
  Out(Format),
  OutputMonad(..),
  ReportT (Report),
  abortWith,
  format,
  getAllOuts,
  toOutput,
  withLang,
  )

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Writer (MonadWriter (tell))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (Foldable (foldl'))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Text.LaTeX.Base.Syntax (
  LaTeX (TeXComm, TeXEnv, TeXRaw),
  TeXArg (FixArg, OptArg),
  )

toOutputL :: Language -> Out LaTeX -> Maybe LaTeX
toOutputL = toOutput . withL

alignOutputL
  :: ([LaTeX] -> LaTeX)
  -> LangM' (ReportT LaTeX IO) a
  -> LangM' (ReportT LaTeX IO) ()
alignOutputL f lr = LangM $ \l -> do
  Report $ do
    os <- liftIO $ getAllOuts (lr `withLang` l)
    xs <- MaybeT . return $ toOutputL l `mapM` os
    tell . (:[]) . Format $ f xs

withL :: Language -> M.Map Language String -> LaTeX
withL l = maybe mempty (TeXRaw . pack) . M.lookup l

toLaTeX :: Monad m => Language -> ReportT LaTeX m a -> m LaTeX
toLaTeX l r = do
  os <- getAllOuts r
  return $ mconcat $ bs os
  where
    bs = fmap $ fromMaybe (error "was rejected") . toOutputL l

getLaTeX :: Monad m => Language -> ReportT LaTeX m a -> m (Either LaTeX LaTeX)
getLaTeX l r = do
  os <- getAllOuts r
  return $ foldl'
    (\xs x -> do
        xs' <- xs
        maybe (Left xs') (return . (xs' <>)) $ toOutputL l x)
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

instance OutputMonad (ReportT LaTeX IO) where
  assertion p o = do
    o
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
  paragraph = alignOutputL ((<> par) . mconcat)
  text = format . TeXRaw . pack . (\xs -> ' ':xs ++ " ")
  refuse xs = xs >> abortWith mempty
  indent = alignOutputL (TeXEnv "quote" [] . mconcat)
  enumerateM f xs = LangM $ \l -> Report $ do
    let xs' = bimap ((`withLang` l) . f) (`withLang` l) <$> xs
    xs'' <- outT l `mapM` xs'
    tell . (:[]) . Format $ TeXEnv "enumerate" [] $ mconcat xs''
    where
      outT l (x, y) = (\x' y' ->
          TeXComm "item" [OptArg $ mconcat x', FixArg $ mconcat y']
          <> newline)
        <$> toOut l x
        <*> toOut l y
      toOut l r = do
        os <- lift $ lift $ getAllOuts r
        MaybeT . return . sequence $ toOutputL l <$> os
  itemizeM = alignOutputL
    (TeXEnv "itemize" [] . foldr (\x y -> TeXComm "item" [] <> x <> y) mempty) . sequence
  latex = format . TeXRaw . pack . ('$':) . (++ "$")
  code = format . TeXEnv "verbatim" [] . TeXRaw . pack
  translated lm = LangM $ \l -> Report . tell . (:[]) . Format  $ withL l lm
