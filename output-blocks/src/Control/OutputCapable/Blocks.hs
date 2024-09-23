{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wwarn=star-is-type #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module provides common skeletons for printing tasks
module Control.OutputCapable.Blocks (
  FunctorTrans (..),
  -- * Report monad
  GenericOut (..),
  GenericReportT (..),
  Language (..),
  Out,
  ReportT,
  Report,
  getAllOuts,
  getOutsWithResult,
  toOutput,
  -- * Monad for translations
  GenericLangM (LangM, unLangM),
  LangM',
  LangM,
  Rated,
  -- * Output monad
  GenericOutputCapable (..),
  OutputCapable,
  enumerate,
  abortWith,
  alignOutput,
  combineReports,
  combineTwoReports,
  format,
  recoverFrom,
  recoverWith,
  toAbort,
  -- * Translation
  ArticleToUse (..),
  english,
  german,
  localise,
  mapLangM,
  multiLang,
  translate,
  translateCode,
  translations,
  -- * Helper functions
  ($=<<),
  multipleChoice,
  printSolutionAndAssert,
  reRefuse,
  singleChoice,
  singleChoiceSyntax,
  continueOrAbort,
  yesNo,
  ) where


import qualified Control.OutputCapable.Blocks.Generic as Generic (
  alignOutput,
  combineReports,
  combineTwoReports,
  toAbort,
  translate,
  translateCode,
  translations,
  )
import qualified Data.Map as M

import Control.Functor.Trans            (FunctorTrans (lift))
import Control.OutputCapable.Blocks.Generic (
  GenericLangM (..),
  GenericOutputCapable (..),
  GenericReportT (..),
  RunnableOutputCapable (..),
  ($=<<),
  ($>>),
  ($>>=),
  abortWith,
  format,
  mapLangM,
  recoverFrom,
  recoverWith,
  runLangMReport,
  )
import Control.OutputCapable.Blocks.Report (
  GenericOut (..),
  Language (..),
  Out,
  ReportT,
  Report,
  getAllOuts,
  getOutsWithResult,
  toOutput,
  )

import Control.Applicative              (Alternative)
import Control.Monad                    (unless, when)
import Control.Monad.State              (State, modify)
import Control.Monad.Writer (
  MonadWriter (tell),
  )
import Data.Containers.ListUtils        (nubOrd)
import Data.Foldable                    (for_)
import Data.List                        (sort)
import Data.Map                         (Map,foldrWithKey)
import Data.Maybe                       (fromMaybe, isJust)
import Data.Ratio                       ((%))
import GHC.Generics                     (Generic)

{-|
If argument is 'True',
it will continue after assertion,
otherwise it will stop if assertion fails.
-}
continueOrAbort :: OutputCapable m => Bool -> Bool -> LangM m -> LangM m
continueOrAbort True  = yesNo
continueOrAbort False = assertion

{-|
In contrast to 'assertion' it will only indicate that a check was performed
and its result.
However, it will not abort.
-}
yesNo :: OutputCapable m => Bool -> LangM m -> LangM m
yesNo p q = do
  paragraph q
  paragraph $ indent $ translatedCode $ flip localise $ translations $
      if p
      then do
        english "Yes."
        german "Ja."
      else do
        english "No."
        german "Nein."
  pure ()

multipleChoice
  :: (OutputCapable m, Ord a)
  => ArticleToUse
  -> Map Language String
  -> Maybe String
  -> Map a Bool
  -> [a]
  -> Rated m
multipleChoice articleToUse what optionalSolutionString solution choices =
  correctnessCheck
  *> exhaustivenessCheck
  *> printSolutionAndAssert articleToUse optionalSolutionString points
  where
    cs = sort $ nubOrd choices
    points = percentPer
      solution
      (toMapping (M.keys solution) cs)
    isCorrect = null [c | c <- cs, c `notElem` valid]
    correctnessCheck = yesNo isCorrect $ multiLang [
      (English, "All given " ++ localise English what ++ " are correct?"),
      (German, "Alle angegebenen " ++ localise German what ++ " sind korrekt?")
      ]
    exhaustivenessCheck =  when isCorrect $ yesNo (cs ==  valid) $ multiLang [
      (English, "The given " ++ localise English what ++ " are exhaustive?"),
      (German, "Die angegebenen " ++ localise German what ++ " sind vollzählig?")
      ]
    valid = M.keys $ M.filter id solution

{-|
Use the specified article.
-}
data ArticleToUse
  = DefiniteArticle
  -- ^ use definite article(s)
  | IndefiniteArticle
  -- ^ use indefinite article(s)
  deriving (Eq, Generic, Read, Show)

printSolutionAndAssert
  :: OutputCapable m
  => ArticleToUse
  -> Maybe String
  -> Rational
  -> Rated m
printSolutionAndAssert articleToUse optionalSolutionString points = do
  for_ optionalSolutionString (\solutionString ->
    when (points /= 1) $ paragraph $ do
      translate $ case articleToUse of
        DefiniteArticle -> do
          english "The correct solution is:"
          german "Die richtige Lösung ist:"
        IndefiniteArticle -> do
          english "A correct solution is:"
          german "Eine richtige Lösung ist:"
      code solutionString
      pure ()
    )
  unless (points >= 1 % 2) $ refuse $ pure ()
  return points

singleChoiceSyntax
  :: (OutputCapable m, Eq a, Show a)
  => Bool
  -> [a]
  -> a
  -> LangM m
singleChoiceSyntax withSolution options choice =
  let assert = continueOrAbort withSolution
  in assert (choice `elem` options) $ translate $ do
    let c = show choice
    english $ "Chosen option " ++ c  ++ " is available?"
    german $ "Gewählte Option " ++ c ++ " ist verfügbar?"

singleChoice
  :: (OutputCapable m, Eq a)
  => ArticleToUse
  -> Map Language String
  -> Maybe String
  -> a
  -> a
  -> Rated m
singleChoice articleToUse what optionalSolutionString solution choice = do
  checkCorrect
  *> printSolutionAndAssert articleToUse optionalSolutionString points
  where
    correct = solution == choice
    points = if correct then 1 else 0
    assert = continueOrAbort $ isJust optionalSolutionString
    checkCorrect = assert correct $ multiLang [
      (English, "Chosen " ++ localise English what ++ " is correct?"),
      (German, "Der/die/das gewählte " ++ localise German what ++ " ist korrekt?")]

{-|
Returns a list stating for each element of the first list
if the element exists within the second list.
-}
toMapping :: Eq a => [a] -> [a] -> [(a, Bool)]
toMapping xs ys = fmap (\x -> (x, x `elem` ys)) xs

{-|
The relative amount of elements in the list
being a member of the map with the same value.
-}
percentPer :: (Eq a, Ord k) => Map k a -> [(k, a)] -> Rational
percentPer xs = (% toInteger (length xs)) . sum
  . fmap (\(k, y) -> if M.lookup k xs == Just y then 1 else 0)

{-
Append some remarks after some rating function.
But re-reject afterwards (if it was rejected by the rating function).
-}
reRefuse
  :: (Alternative m, Monad m, OutputCapable m)
  => Rated m
  -> LangM m
  -> Rated m
reRefuse xs ys =
  recoverWith (pure 0) xs
    $>>= \x -> ys
    $>> either (refuse (pure ()) *>) pure x

multiLang :: OutputCapable m => [(Language, String)] -> LangM m
multiLang xs = translated $ \l ->
  fromMaybe "" $ lookup l xs

localise :: Language -> Map Language String -> String
localise l lm = fromMaybe nonExistent $ M.lookup l lm
  where
    nonExistent
      | null lm   = error "missing translation"
      | otherwise = snd $ M.findMin lm

{-|
This is a specified version of 'Generic.translate'
which enforces the usage pattern.
You should always prefer this specified version over the generic.
-}
translate
  :: GenericOutputCapable l m
  => State (Map l String) ()
  -> GenericLangM l m ()
translate = Generic.translate

{-|
This is a specified version of 'Generic.translateCode'
which enforces the usage pattern.
You should always prefer this specified version over the generic.
-}
translateCode
  :: GenericOutputCapable l m
  => State (Map l String) ()
  -> GenericLangM l m ()
translateCode = Generic.translateCode

{-|
This is a specified version of 'Generic.translations'
which enforces the usage pattern.
You should always prefer this specified version over the generic.
-}
translations :: State (Map l a) () -> Map l a
translations = Generic.translations

english :: String -> State (Map Language String) ()
english = modify . M.insertWith (flip (++)) English

german :: String -> State (Map Language String) ()
german = modify . M.insertWith (flip (++)) German

type LangM' m a = GenericLangM Language m a
type LangM m = LangM' m ()
type Rated m = LangM' m Rational

enumerate
  :: GenericOutputCapable l m
  => (k -> String)
  -> (a -> String)
  -> Map k a
  -> GenericLangM l m ()
enumerate f g m = enumerateM (text . f) (M.toList $ text . g <$> m)

type OutputCapable m = GenericOutputCapable Language m

out :: Monad m => GenericOut l o -> GenericLangM l (GenericReportT l o m) ()
out = lift . Report . tell . (:[])

instance (l ~ Language)
  => GenericOutputCapable l (GenericReportT l (IO ()) IO)
  where
  assertion p m = (if p then id else refuse)
    $ yesNo p m
  image         = format . putStr . ("file: " ++)
  images g f    = format . putStrLn . foldrWithKey
    (\k x rs -> g k ++ ". file: " ++ f x ++ '\n' : rs)
    ""
  paragraph     = (*> format (putStrLn ""))
  text          = format . putStr
  enumerateM p  = foldl
    (\o (x, e) -> paragraph $ do o; p x; format $ putStr "  "; e; pure ())
    (pure ())
  itemizeM      = foldl
    (\o x -> paragraph $ do o; format $ putStr " -  "; x; pure ())
    (pure ())
  indent xs     = do
    format $ putStr ">>>>"
    xs
    format $ putStrLn "<<<<"
    pure ()
  refuse        = toAbort
  latex         = format . putStrLn . ("LaTeX: " ++)
  code          = format . putStr . (\xs -> " <" ++ xs ++ "> ")
  translatedCode lm =
    out (Localised $ putStr . (\xs -> " <" ++ xs ++ "> ") . lm)
  translated lm = out (Localised $ putStr . lm)

instance l ~ Language
  => RunnableOutputCapable l (GenericReportT l (IO ()) IO)
  where
  type RunMonad l (GenericReportT l (IO ()) IO) = IO
  type Output l (GenericReportT l (IO ()) IO) = IO ()
  runLangM = runLangMReport (return ()) (>>)

{-|
This is a specified version of 'Generic.combineReports'
which enforces the usage pattern.
You should always prefer this specified version over the generic.
-}
combineReports
  :: Monad m
  => ([[o]] -> o)
  -> [GenericLangM l (GenericReportT l o m) ()]
  -> GenericLangM l (GenericReportT l o m) ()
combineReports = Generic.combineReports

{-|
This is a specified version of 'Generic.alignOutput'
which enforces the usage pattern.
You should always prefer this specified version over the generic.
-}
alignOutput
  :: Monad m
  => ([o] -> o)
  -> GenericLangM l (GenericReportT l o m) ()
  -> GenericLangM l (GenericReportT l o m) ()
alignOutput = Generic.alignOutput

{-|
This is a specified version of 'Generic.combineTwoReports'
which enforces the usage pattern.
You should always prefer this specified version over the generic.
-}
combineTwoReports
  :: Monad m
  => ([o] -> [o] -> o)
  -> GenericLangM l (GenericReportT l o m) ()
  -> GenericLangM l (GenericReportT l o m) ()
  -> GenericLangM l (GenericReportT l o m) ()
combineTwoReports = Generic.combineTwoReports

{-|
This is a specified version of 'Generic.toAbort'
which enforces the usage pattern.
You should always prefer this specified version over the generic.
-}
toAbort
  :: Monad m
  => GenericLangM l (GenericReportT l o m) ()
  -> GenericLangM l (GenericReportT l o m) ()
toAbort = Generic.toAbort
