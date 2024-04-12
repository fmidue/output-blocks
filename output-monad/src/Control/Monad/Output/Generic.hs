{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{- |
This module provides common skeletons for creating multilingual output.
Provided interfaces can be used to generate simultaneous multilingual output
as well as output which can be rendered in different Languages
(when the specific Language is provided)
-}
module Control.Monad.Output.Generic (
  FunctorTrans (..),
  -- * Monad for translations
  GenericLangM (LangM, unLangM),
  GenericReportT (..),
  -- * Output monad
  GenericOutputMonad (..),
  RunnableOutputMonad (..),
  abortWith,
  alignOutput,
  combineReports,
  combineTwoReports,
  format,
  recoverFrom,
  recoverWith,
  toAbort,
  -- * Translation
  mapLangM,
  -- * Helper functions
  ($=<<),
  ($>>),
  ($>>=),
  evalLangM,
  execLangM,
  runLangMReport,
  runLangMReportMultiLang,
  translate,
  translateCode,
  translations,
  withLang,
  ) where

import qualified Control.Monad.Output.Report.Generic as Report (
  alignOutput,
  combineReports,
  combineTwoReports,
  toAbort,
  )

import qualified Data.Map                         as M (empty, lookup)

import Control.Monad.Output.Report.Generic (
  GenericOut (..),
  GenericReportT (..),
  getOutsWithResult,
  )


import Control.Applicative              (Alternative ((<|>)))
import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad                    (unless, void)
import Control.Monad.State              (State, execState)
import Control.Monad.Writer (
  MonadWriter (tell),
  )
import Data.Kind                        (Type)
import Data.Foldable                    (foldl', sequenceA_, traverse_)
import Data.Functor.Identity            (Identity (Identity))
import Data.Map                         (Map)
import Data.Maybe                       (fromMaybe)

newtype GenericLangM l m a = LangM { unLangM :: m a }
  deriving (Applicative, Functor)

instance FunctorTrans (GenericLangM l) where
  lift = LangM

class (Applicative m, Ord l) => GenericOutputMonad l m where
  -- | for assertions, i.e. expected behaviour is explanation
  -- (and abortion on 'False')
  assertion  :: Bool -> GenericLangM l m () -> GenericLangM l m ()
  -- | for printing a single image from file
  image      :: FilePath -> GenericLangM l m ()
  -- | for printing multiple images using the given map
  images     :: (k -> String) -> (a -> FilePath) -> Map k a -> GenericLangM l m ()
  -- | for a complete paragraph
  paragraph  :: GenericLangM l m () -> GenericLangM l m ()
  -- | should abort at once
  refuse     :: GenericLangM l m () -> GenericLangM l m ()
  -- | for displaying text
  text       :: String -> GenericLangM l m ()
  text = translated . const
  -- | for an enumerated sequence of elements
  enumerateM
    :: (a -> GenericLangM l m ())
    -> [(a, GenericLangM l m ())]
    -> GenericLangM l m ()
  -- | for an unenumerated sequence of elements
  itemizeM   :: [GenericLangM l m ()] -> GenericLangM l m ()
  -- | for indentation
  indent     :: GenericLangM l m () -> GenericLangM l m ()
  -- | for LaTeX-Math code (i.e. without surrounding @$@)
  latex      :: String -> GenericLangM l m ()
  -- | for fixed width fonts (i.e. typewriter style)
  code       :: String -> GenericLangM l m ()
  code = translatedCode . const
  -- | same as 'code', but with different translations
  translatedCode :: (l -> String) -> GenericLangM l m ()
  -- | for displaying text with translations
  translated :: (l -> String) -> GenericLangM l m ()

infixr 0 $=<<, $>>=, $>>

($=<<) :: Monad m => (a -> GenericLangM l m b) -> m a -> GenericLangM l m b
f $=<< x = LangM $ x >>= unLangM . f

($>>=)
  :: Monad m
  => GenericLangM l m a
  -> (a -> GenericLangM l m b)
  -> GenericLangM l m b
x $>>= f = LangM $ unLangM x >>= unLangM . f

($>>)
  :: Monad m
  => GenericLangM l m a
  -> GenericLangM l m b
  -> GenericLangM l m b
x $>> y = LangM $ unLangM x >> unLangM y

class (GenericOutputMonad l m, Monad (RunMonad l m))
  => RunnableOutputMonad l m where
  -- | the monad handling multilingual output
  type RunMonad l m :: Type -> Type
  type Output l m
  runLangM
    :: GenericLangM l m a
    -> RunMonad l m (Maybe a, l -> Output l m)

execLangM
  :: RunnableOutputMonad l m
  => GenericLangM l m a
  -> RunMonad l m (l -> Output l m)
execLangM lm = do
  ~(_, output) <- runLangM lm
  return output

evalLangM
  :: RunnableOutputMonad l m
  => GenericLangM l m a
  -> RunMonad l m (Maybe a)
evalLangM lm = do
  ~(result, _) <- runLangM lm
  return result

withLang
  :: (RunnableOutputMonad l m, Output l m ~ RunMonad l m b)
  => GenericLangM l m a
  -> l
  -> RunMonad l m (Maybe a)
withLang xs l = do
  (r, o) <- runLangM xs
  o l
  return r

recoverFrom
  :: Alternative m
  => GenericLangM l m ()
  -> GenericLangM l m ()
recoverFrom x = LangM $ unLangM x <|> pure ()

recoverWith
  :: Alternative m
  => a
  -> GenericLangM l m b
  -> GenericLangM l m (Either a b)
recoverWith x m = LangM $ (Right <$> unLangM m) <|> pure (Left x)

combineLangMs
  :: ([GenericReportT l o m a] -> GenericReportT l o m b)
  -> [GenericLangM l (GenericReportT l o m) a]
  -> GenericLangM l (GenericReportT l o m) b
combineLangMs f oms = LangM $ f $ map unLangM oms

combineLangM
  :: (GenericReportT l o m a -> GenericReportT l o m b -> GenericReportT l o m c)
  -> GenericLangM l (GenericReportT l o m) a
  -> GenericLangM l (GenericReportT l o m) b
  -> GenericLangM l (GenericReportT l o m) c
combineLangM f x y = LangM $ f (unLangM x) (unLangM y)

{-|
Combines the output of the list of given reports using the provided function.
Output is provided for all reports up to including the first failing report.
-}
combineReports
  :: Monad m
  => ([[o]] -> o)
  -> [GenericLangM l (GenericReportT l o m) a]
  -> GenericLangM l (GenericReportT l o m) ()
combineReports f = combineLangMs (Report.combineReports f)

alignOutput
  :: Monad m
  => ([o] -> o)
  -> GenericLangM l (GenericReportT l o m) a
  -> GenericLangM l (GenericReportT l o m) ()
alignOutput f = void . mapLangM (Report.alignOutput f)

{-|
Combines the output of the two given reports using the provided functions.

If the execution aborts on the first report the second report is treated
as if has not produced any output.
-}
combineTwoReports
  :: Monad m
  => ([o] -> [o] -> o)
  -> GenericLangM l (GenericReportT l o m) a
  -> GenericLangM l (GenericReportT l o m) b
  -> GenericLangM l (GenericReportT l o m) ()
combineTwoReports = combineLangM . Report.combineTwoReports

out :: Monad m => GenericOut l o -> GenericLangM l (GenericReportT l o m) ()
out = lift . Report . tell . (:[])

format :: Monad m => o -> GenericLangM l (GenericReportT l o m) ()
format = out . Format

abortWith :: Monad m => o -> GenericLangM l (GenericReportT l o m) ()
abortWith d = toAbort $ format d

toAbort
  :: Monad m
  => GenericLangM l (GenericReportT l o m) a
  -> GenericLangM l (GenericReportT l o m) b
toAbort = mapLangM Report.toAbort

mapLangM :: (m a -> m b) -> GenericLangM l m a -> GenericLangM l m b
mapLangM f om = LangM $ f $ unLangM om

instance Ord l => GenericOutputMonad l Maybe where
  assertion b _   = unless b $ lift Nothing
  image _         = pure ()
  images _ _ _    = pure ()
  paragraph xs    = xs
  refuse xs       = xs *> lift Nothing
  text _          = pure ()
  enumerateM f xs = (\(x, y) -> f x *> y) `traverse_` xs
  itemizeM        = sequenceA_
  indent xs       = xs
  latex _         = pure ()
  code _          = pure ()
  translatedCode _ = pure ()
  translated _    = pure ()

instance Ord l => RunnableOutputMonad l Maybe where
  type RunMonad l Maybe = Identity
  type Output l Maybe = ()
  runLangM        = Identity . (, const ()) . unLangM

translate
  :: GenericOutputMonad language m
  => State (Map language String) a
  -> GenericLangM language m ()
translate xs = translated $ \l ->
  fromMaybe "" $ M.lookup l $ translations xs

translateCode
  :: GenericOutputMonad language m
  => State (Map language String) a
  -> GenericLangM language m ()
translateCode xs = translatedCode $ \l ->
  fromMaybe "" $ M.lookup l $ translations xs

translations :: State (Map language a1) a2 -> Map language a1
translations = flip execState M.empty


{-|
Provided a neutral element and a function to combine generated output
this function will evaluate 'GenericLangM' and combine the output.

A specific output can be rendered when a language is provided
to the returned function.
-}
runLangMReport
  :: Monad m
  => o
  -> (o -> o -> o)
  -> GenericLangM l (GenericReportT l o m) a
  -> m (Maybe a, l -> o)
runLangMReport neutral f lm = do
  (r, os) <- getOutsWithResult $ unLangM lm
  let output l = foldl' (toOutput' l) neutral os
  return (r, output)
  where
    toOutput' l xs x =
      case x of
        Format o -> f xs o
        Localised m -> f xs (m l)

{-|
Provided a neutral element, a function to combine generated output,
and a function to remap translations into output
this function will evaluate 'GenericLangM' and combine the output.

The provided output unifies multilingual output as one.
This could for instance be with local definitions of translated parts.
-}
runLangMReportMultiLang
  :: Monad m
  => o
  -> (o -> o -> o)
  -> ((l -> o) -> o)
  -> GenericLangM l (GenericReportT l o m) a
  -> m (Maybe a, o)
runLangMReportMultiLang neutral f toO lm = do
  (r, os) <- getOutsWithResult $ unLangM lm
  let output = foldl' toOutput' neutral os
  return (r, output)
  where
    toOutput' xs x =
      case x of
        Format o -> f xs o
        Localised m -> f xs $ toO m
