{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wwarn=star-is-type #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module provides common skeletons for printing tasks
module Control.Monad.Output (
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
  GenericOutputMonad (..),
  OutputMonad,
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
  singleChoice,
  singleChoiceSyntax,
  continueOrAbort,
  yesNo,
  ) where


import qualified Control.Monad.Output.Generic     as Generic (
  translate,
  translateCode,
  translations,
  )
import qualified Data.Map as M

import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad.Output.Generic (
  GenericLangM (..),
  GenericOutputMonad (..),
  GenericReportT (..),
  RunnableOutputMonad (..),
  ($=<<),
  abortWith,
  alignOutput,
  combineReports,
  combineTwoReports,
  format,
  recoverFrom,
  recoverWith,
  runLangMReport,
  toAbort,
  mapLangM,
  )
import Control.Monad.Output.Report (
  GenericOut (..),
  Language (..),
  Out,
  ReportT,
  Report,
  getAllOuts,
  getOutsWithResult,
  toOutput,
  )

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

{-|
If argument is 'True',
it will continue after assertion,
otherwise it will stop if assertion fails.
-}
continueOrAbort :: OutputMonad m => Bool -> Bool -> LangM m -> LangM m
continueOrAbort True  = yesNo
continueOrAbort False = assertion

{-|
In contrast to 'assertion' it will only indicate that a check was performed
and its result.
However, it will not abort.
-}
yesNo :: OutputMonad m => Bool -> LangM m -> LangM m
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
  :: (OutputMonad m, Ord a)
  => Map Language String
  -> Maybe String
  -> Map a Bool
  -> [a]
  -> Rated m
multipleChoice what optionalSolutionString solution choices =
  correctnessCheck
  *> exhaustivenessCheck
  *> printSolutionAndAssert optionalSolutionString points
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

printSolutionAndAssert
  :: OutputMonad m
  => Maybe String
  -> Rational
  -> Rated m
printSolutionAndAssert optionalSolutionString points = do
  for_ optionalSolutionString (\solutionString ->
    when (points /= 1) $ paragraph $ do
      translate $ do
        english "The correct solution is:"
        german "Die richtige Lösung ist:"
      code solutionString
      pure ()
    )
  unless (points >= 1 % 2) $ refuse $ pure ()
  return points

singleChoiceSyntax
  :: (OutputMonad m, Eq a, Show a)
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
  :: (OutputMonad m, Eq a)
  => Map Language String
  -> Maybe String
  -> a
  -> a
  -> Rated m
singleChoice what optionalSolutionString solution choice = do
  checkCorrect
  *> printSolutionAndAssert optionalSolutionString points
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

multiLang :: OutputMonad m => [(Language, String)] -> LangM m
multiLang xs = translated $ \l ->
  fromMaybe "" $ lookup l xs

localise :: Language -> Map Language String -> String
localise l lm = fromMaybe nonExistent $ M.lookup l lm
  where
    nonExistent
      | null lm   = error "missing translation"
      | otherwise = snd $ M.findMin lm

translate :: OutputMonad m => State (Map Language String) a -> LangM m
translate = Generic.translate

translateCode :: OutputMonad m => State (Map Language String) a -> LangM m
translateCode = Generic.translateCode

translations :: State (Map k a1) a2 -> Map k a1
translations = Generic.translations

english :: String -> State (Map Language String) ()
english = modify . M.insertWith (flip (++)) English

german :: String -> State (Map Language String) ()
german = modify . M.insertWith (flip (++)) German

type LangM' m a = GenericLangM Language m a
type LangM m = LangM' m ()
type Rated m = LangM' m Rational

enumerate
  :: OutputMonad m
  => (k -> String)
  -> (a -> String)
  -> Map k a
  -> LangM m
enumerate f g m = enumerateM (text . f) (M.toList $ text . g <$> m)

type OutputMonad m = GenericOutputMonad Language m

out :: Monad m => GenericOut l o -> GenericLangM l (GenericReportT l o m) ()
out = lift . Report . tell . (:[])

instance (l ~ Language)
  => GenericOutputMonad l (GenericReportT l (IO ()) IO)
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
  => RunnableOutputMonad l (GenericReportT l (IO ()) IO)
  where
  type RunMonad l (GenericReportT l (IO ()) IO) = IO
  type Output l (GenericReportT l (IO ()) IO) = IO ()
  runLangM = runLangMReport (return ()) (>>)
