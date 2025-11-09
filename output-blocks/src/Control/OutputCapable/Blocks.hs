{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
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
  collapsed,
  english,
  german,
  localise,
  mapLangM,
  multiLang,
  translate,
  translateCode,
  translations,
  -- * Helper functions
  MinimumThreshold (..),
  Punishment (..),
  TargetedCorrect (..),
  ($=<<),
  extendedMultipleChoice,
  multipleChoice,
  multipleChoiceSyntax,
  printSolutionAndAssert,
  printSolutionAndAssertMinimum,
  reRefuse,
  reRefuseLangM,
  singleChoice,
  singleChoiceSyntax,
  continueOrAbort,
  yesNo,
  ) where


import qualified Control.OutputCapable.Blocks.Generic as Generic (
  alignOutput,
  collapsed,
  combineReports,
  combineTwoReports,
  toAbort,
  translate,
  translateCode,
  translations,
  )
import qualified Data.Map as M

import Autolib.Hash                     (Hashable)
import Autolib.Reader                   (Reader)
import Autolib.ToDoc                    (ToDoc)
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
import Control.Monad                    (unless, void, when)
import Control.Monad.State              (State, modify)
import Control.Monad.Writer (
  MonadWriter (tell),
  )
import Data.Containers.ListUtils        (nubOrd)
import Data.Data                        (Data)
import Data.Foldable                    (for_, traverse_)
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

{-|
A 'Rational' number indicating the minimal threshold.
-}
newtype MinimumThreshold = MinimumThreshold {
  unMinimumThreshold :: Rational
  }

{-|
A 'Rational' number indicating the punishment.
-}
newtype Punishment = Punishment {
  unPunishment :: Rational
  }

{-|
A 'Int' number indicating expected correct answers.
-}
newtype TargetedCorrect = TargetedCorrect {
  unTargetedCorrect :: Int
  }

{-|
Outputs feedback on syntax of a multiple choice submission.
Depending on chosen parameters it might reject the submission.
-}
multipleChoiceSyntax
  :: (OutputCapable m, Ord a, Show a)
  => Bool
  -- ^ whether to continue after check (i.e. do not reject wrong answers)
  -> [a]
  -- ^ possible answers
  -> [a]
  -- ^ the submission to evaluate
  -> LangM m
multipleChoiceSyntax withSolution options =
  traverse_ (singleChoiceSyntax withSolution options) . nubOrd

{-|
Evaluates multiple choice submissions
by rejecting correctness below 50 percent.
(see 'extendedMultipleChoice')
-}
multipleChoice
  :: (OutputCapable m, Ord a)
  => Map Language String
  -- ^ what is asked for
  -> Maybe (ArticleToUse, String)
  -- ^ the correct solution to show,
  -- and the article kind indicating if multiple different solutions could be possible
  -> Map a Bool
  -- ^ possible answers and if they are correct
  -> [a]
  -- ^ the submission to evaluate
  -> Rated m
multipleChoice what optionalSolution solution =
  extendedMultipleChoice
  (MinimumThreshold (1 % 2))
  (Punishment 0)
  (TargetedCorrect (length solution))
  what
  optionalSolution
  solution
  . foldr (`M.insert` True) (M.filter not solution)

{-|
Evaluates multiple choice submissions
by rejecting correctness below a minimum threshold.


The following preconditions need to hold before calling this function
but are not checked:

 * targeted correct is at least one
   and not larger than the amount of possible answers
-}
extendedMultipleChoice
  :: (OutputCapable m, Ord a)
  => MinimumThreshold
  -- ^ the minimum threshold of achieved points
  -> Punishment
  -- ^ points to subtract per wrong answer
  -> TargetedCorrect
  -- ^ how many correct answers have to be given within the submission
  -- in order to achieve full points
  -> Map Language String
  -- ^ what is asked for
  -> Maybe (ArticleToUse, String)
  -- ^ the correct solution to show,
  -- and the article kind indicating if multiple different solutions could be possible
  -> Map a Bool
  -- ^ possible answers and if they are correct
  -> Map a Bool
  -- ^ the submission to evaluate
  -> Rated m
extendedMultipleChoice
  minimumPoints
  punishment
  targeted
  what
  optionalSolution
  solution
  choices
  = correctnessCheck
  *> exhaustivenessCheck
  *> printSolutionAndAssertMinimum
    minimumPoints
    True
    optionalSolution
    points
  where
    madeUp = M.difference choices solution
    chosenTrue = M.intersection solution $ M.filter id choices
    isCorrect = M.null madeUp && and chosenTrue
    points = gradeMultipleChoice punishment targeted solution choices
    correctnessCheck = yesNo isCorrect $ multiLang [
      (English, "All indicated " ++ localise English what ++ " are correct?"),
      (German, "Alle angegebenen " ++ localise German what ++ " sind korrekt?")
      ]
    answers = M.intersectionWith (==) solution choices
    isComplete = and answers && length answers >= unTargetedCorrect targeted
    exhaustivenessCheck = when isCorrect
      $ yesNo isComplete $ multiLang [
      (English, "The indicated " ++ localise English what ++ " are exhaustive?"),
      (German, "Die angegebenen " ++ localise German what ++ " sind vollzählig?")
      ]

{-|
Calculates points based on the portion of correct choices.

Note that invalid entries within the submission list
(i.e. which are not covered by the possible answers map)
are punished like wrong answers.

The following preconditions need to hold before calling this function
but are not checked:

 * targeted correct is at least one
   and not larger than the amount of possible answers
-}
gradeMultipleChoice
  :: Ord k
  => Punishment
  -- ^ how many points to subtract per wrong answer given
  -> TargetedCorrect
  -- ^ how many of all possible correct answers are considered exhaustive
  -> Map k Bool
  -- ^ possible answers and if they are correct
  -> Map k Bool
  -- ^ submission
  -> Rational
gradeMultipleChoice Punishment {..} TargetedCorrect {..} solution choices =
  max 0
    $ min 1 (toInteger (length correct) % toInteger unTargetedCorrect)
    - toInteger (length incorrect) % 1 * unPunishment
  where
    (correct, incorrect) =
      M.partitionWithKey (\k -> (== M.lookup k solution) . Just) choices


{-|
Use the specified article.
-}
data ArticleToUse
  = DefiniteArticle
  -- ^ use definite article(s)
  | IndefiniteArticle
  -- ^ use indefinite article(s)
  deriving (Data, Eq, Generic, Hashable, Read, Reader, Show, ToDoc)

{-|
Outputs the correct solution (if given)
when achieved points are less than 100 percent.
No points are distributed if not at least 50 percent are achieved.
(see 'printSolutionAndAssertMinimum')
-}
printSolutionAndAssert
  :: OutputCapable m
  => Bool
  -- ^ whether to mention exhaustiveness
  -- (use "correct and exhaustive" instead of just "correct" in output text)
  -> Maybe (ArticleToUse, String)
  -- ^ the correct solution to show,
  -- and the article kind indicating if multiple different solutions could be possible
  -> Rational
  -- ^ points achieved
  -> Rated m
printSolutionAndAssert = printSolutionAndAssertMinimum
  $ MinimumThreshold (1 % 2)

{-|
Outputs the correct solution (if given)
when achieved points are less than 100 percent.
No points are distributed if they do not reach the minimum threshold.
-}
printSolutionAndAssertMinimum
  :: OutputCapable m
  => MinimumThreshold
  -- ^ the minimum threshold of achieved points
  -> Bool
  -- ^ whether to mention exhaustiveness
  -- (use "correct and exhaustive" instead of just "correct" in output text)
  -> Maybe (ArticleToUse, String)
  -- ^ the correct solution to show,
  -- and the article kind indicating if multiple different solutions could be possible
  -> Rational
  -- ^ points achieved
  -> Rated m
printSolutionAndAssertMinimum
  minimumPoints
  mentionExhaustiveness
  optionalSolution
  points
  = do
  for_ optionalSolution (\(articleToUse, solutionString) ->
    when (points /= 1) $ paragraph $ do
      translate $ case articleToUse of
        DefiniteArticle -> do
          english $ if mentionExhaustiveness
            then "The correct and exhaustive solution is:"
            else "The correct solution is:"
          german $ if mentionExhaustiveness
            then "Die korrekte und vollzählige Lösung ist:"
            else "Die korrekte Lösung ist:"
        IndefiniteArticle -> do
          english $ if mentionExhaustiveness
            then "A correct and exhaustive solution is:"
            else "A correct solution is:"
          german $ if mentionExhaustiveness
            then "Eine korrekte und vollzählige Lösung ist:"
            else "Eine korrekte Lösung ist:"
      code solutionString
      pure ()
    )
  unless (points >= unMinimumThreshold minimumPoints) $ refuse $ pure ()
  return points

{-|
Outputs feedback on syntax of a single choice submission.
Depending on chosen parameters it might reject the submission.
-}
singleChoiceSyntax
  :: (OutputCapable m, Eq a, Show a)
  => Bool
  -- ^ whether to continue after check (i.e. do not reject wrong answers)
  -> [a]
  -- ^ possible answers
  -> a
  -- ^ the submission to evaluate
  -> LangM m
singleChoiceSyntax withSolution options choice =
  let assert = continueOrAbort withSolution
  in assert (choice `elem` options) $ translate $ do
    let c = show choice
    english $ "Chosen option " ++ c  ++ " is available?"
    german $ "Gewählte Option " ++ c ++ " ist verfügbar?"

{-|
Outputs feedback and rates a single choice submission.
-}
singleChoice
  :: (OutputCapable m, Eq a)
  => Map Language String
  -- ^ what is asked for
  -> Maybe (ArticleToUse, String)
  -- ^ the correct solution to show,
  -- and the article kind indicating if multiple different solutions could be possible
  -> a
  -- ^ the correct answer
  -> a
  -- ^ the submission to evaluate
  -> LangM m
singleChoice what optionalSolution solution choice = void $
  checkCorrect
  *> printSolutionAndAssert False optionalSolution points
  where
    correct = solution == choice
    points = if correct then 1 else 0
    assert = continueOrAbort $ isJust optionalSolution
    checkCorrect = assert correct $ multiLang [
      (English, "Chosen " ++ localise English what ++ " is correct?"),
      (German, "Der/die/das gewählte " ++ localise German what ++ " ist korrekt?")]

{-|
Append some remarks after some rating function.
But re-reject afterwards (if it was rejected by the rating function).
-}
reRefuse
  :: (Alternative m, Monad m, OutputCapable m)
  => Rated m
  -> LangM m
  -> Rated m
reRefuse = reRefuseWith 0

{-|
Append some remarks after a potential rejection.
But re-reject afterwards (if it was rejected before).

@since 0.4.0.3
-}
reRefuseLangM
  :: (Alternative m, Monad m, OutputCapable m)
  => LangM m
  -> LangM m
  -> LangM m
reRefuseLangM = reRefuseWith ()

{-|
Append some remarks after a potential rejection.
But re-reject afterwards (if it was rejected before).
-}
reRefuseWith :: (Alternative m, Monad m, GenericOutputCapable l m)
  => b
  -> GenericLangM l m b
  -> GenericLangM l m a
  -> GenericLangM l m b
reRefuseWith bottom xs ys =
  recoverWith (pure bottom) xs
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
This is a more specific version of 'Generic.translate'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
translate
  :: GenericOutputCapable l m
  => State (Map l String) ()
  -> GenericLangM l m ()
translate = Generic.translate

{-|
This is a more specific version of 'Generic.translateCode'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
translateCode
  :: GenericOutputCapable l m
  => State (Map l String) ()
  -> GenericLangM l m ()
translateCode = Generic.translateCode

{-|
This is a more specific version of 'Generic.translations'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
translations :: State (Map l a) () -> Map l a
translations = Generic.translations

{-|
This is a more specific version of 'Generic.collapsed'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
collapsed
  :: GenericOutputCapable l m
  => Bool
  -> Map l String
  -> GenericLangM l m ()
  -> GenericLangM l m ()
collapsed = Generic.collapsed

{-|
Provide an English translation
to be appended after previous English translations.
-}
english :: String -> State (Map Language String) ()
english = modify . M.insertWith (flip (++)) English

{-|
Provide an German translation
to be appended after previous German translations.
-}
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
  folded b t c = do
    format $ putStr $ "(" ++ (if b then "+" else "-") ++ ") "
    translated t
    format $ putStrLn ""
    unless b (indent c)
    pure ()
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
This is a more specific version of 'Generic.combineReports'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
combineReports
  :: Monad m
  => ([[o]] -> o)
  -> [GenericLangM l (GenericReportT l o m) ()]
  -> GenericLangM l (GenericReportT l o m) ()
combineReports = Generic.combineReports

{-|
This is a more specific version of 'Generic.alignOutput'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
alignOutput
  :: Monad m
  => ([o] -> o)
  -> GenericLangM l (GenericReportT l o m) ()
  -> GenericLangM l (GenericReportT l o m) ()
alignOutput = Generic.alignOutput

{-|
This is a more specific version of 'Generic.combineTwoReports'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
combineTwoReports
  :: Monad m
  => ([o] -> [o] -> o)
  -> GenericLangM l (GenericReportT l o m) ()
  -> GenericLangM l (GenericReportT l o m) ()
  -> GenericLangM l (GenericReportT l o m) ()
combineTwoReports = Generic.combineTwoReports

{-|
This is a more specific version of 'Generic.toAbort'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
toAbort
  :: Monad m
  => GenericLangM l (GenericReportT l o m) ()
  -> GenericLangM l (GenericReportT l o m) ()
toAbort = Generic.toAbort
