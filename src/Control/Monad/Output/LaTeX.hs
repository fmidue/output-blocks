{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Output.LaTeX where

import qualified Data.Map as M

import Control.Monad.Output (
  Language,
  LangM' (LangM),
  Out(Format),
  OutputMonad(..),
  Report (Report),
  abortWith,
  format,
  getAllOuts,
  toOutput,
  withLang,
  )

import Control.Monad.IO.Class (MonadIO (liftIO))
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
import System.IO.Unsafe (unsafePerformIO)

toOutputL :: Language -> Out LaTeX -> Maybe LaTeX
toOutputL = toOutput . withL

alignOutputL
  :: ([LaTeX] -> LaTeX)
  -> LangM' (Report LaTeX) a
  -> LangM' (Report LaTeX) ()
alignOutputL f lr = LangM $ \l -> do
  Report $ do
    xs  <- MaybeT . return . sequence $ toOutputL l <$> getAllOuts (lr `withLang` l)
    tell . (:[]) . Format $ f xs

withL :: Language -> M.Map Language String -> LaTeX
withL l = maybe mempty (TeXRaw . pack) . M.lookup l

toLaTeX :: Language -> Report LaTeX a -> LaTeX
toLaTeX l r = mconcat bs
  where
    bs = fromMaybe (error "was rejected") . toOutputL l <$> getAllOuts r

getLaTeX :: Language -> Report LaTeX a -> Either LaTeX LaTeX
getLaTeX l r = foldl'
  (\xs x -> do
      xs' <- xs
      maybe (Left xs') (return . (xs' <>)) $ toOutputL l x)
  (Right mempty)
  $ getAllOuts r

par :: LaTeX
par = TeXRaw "\n\n"

newline :: LaTeX
newline = TeXRaw "\n"

transformFile :: String -> String
transformFile []     = []
transformFile "svg"  = "pdf"
transformFile (x:xs) = x:transformFile xs

instance MonadIO (Report LaTeX) where
  liftIO = return . unsafePerformIO

instance OutputMonad (Report LaTeX) where
  assertion p o = do
    o
    if p
      then format . TeXEnv "quote" [] $ TeXRaw "Yes."
      else abortWith . TeXEnv "quote" [] $ TeXRaw "No."
  enumerate f g = format . TeXEnv "enumerate" [] .
    M.foldrWithKey
    (\k x -> (TeXComm "item" [OptArg $ TeXRaw $ pack $ f k,
                              FixArg $ TeXRaw $ pack $ g x] <>))
    mempty
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
    xs'' <- sequence $ outT l <$> xs'
    tell . (:[]) . Format $ TeXEnv "enumerate" [] $ mconcat xs''
    where
      outT l (x, y) = (\x' y' ->
          TeXComm "item" [OptArg $ mconcat x', FixArg $ mconcat y']
          <> newline)
        <$> toOut l x
        <*> toOut l y
      toOut l r = MaybeT . return . sequence $ toOutputL l <$> getAllOuts r
  itemizeM = alignOutputL
    (TeXEnv "itemize" [] . foldr (\x y -> TeXComm "item" [] <> x <> y) mempty) . sequence
  latex = format . TeXRaw . pack . ('$':) . (++ "$")
  code = format . TeXEnv "verbatim" [] . TeXRaw . pack
  translated lm = LangM $ \l -> Report . tell . (:[]) . Format  $ withL l lm
