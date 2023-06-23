{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wwarn=star-is-type #-}
-- | This module provides common skeletons for printing tasks
module Control.Monad.Output (
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
  -- * Isolated OutputMonad
  IsolatedOutput (..),
  runIsolated,
  -- * Translation
  english,
  german,
  localise,
  mapLangM,
  multiLang,
  translate,
  translations,
  -- * Helper functions
  multipleChoice,
  printSolutionAndAssert,
  singleChoice,
  singleChoiceSyntax,
  continueOrAbort,
  yesNo,
  ) where

import qualified Control.Monad.Report as Report (
  alignOutput,
  combineReports,
  combineTwoReports,
  toAbort,
  )

import qualified Data.Map as M

import Control.Monad.Report (
  GenericOut (..),
  GenericReportT (..),
  Language (..),
  Out,
  ReportT,
  Report,
  getAllOuts,
  getOutsWithResult,
  toOutput,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Exception                (throwIO)
import Control.Exception.Base           (Exception, displayException)
import Control.Monad                    (unless, when)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.State              (State, execState, modify)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Maybe        (MaybeT (MaybeT))
import Control.Monad.Writer (
  MonadWriter (tell),
  )
import Control.Monad.Catch              (MonadCatch(catch))
import Data.Containers.ListUtils        (nubOrd)
import Data.Foldable                    (for_, sequenceA_, traverse_)
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
  paragraph $ indent $ localised_ code $ translations $
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
multipleChoice what msolutionString solution choices =
  correctnessCheck
  *> exhaustivenessCheck
  *> printSolutionAndAssert msolutionString points
  where
    cs = sort $ nubOrd choices
    points = percentPer
      solution
      (toMapping (M.keys solution) cs)
    isCorrect = null [c | c <- cs, c `notElem` valid]
    correctnessCheck = yesNo isCorrect $ multiLang [
      (English, "Given " ++ localise English what ++ " are correct?"),
      (German, "Die angegebenen " ++ localise German what ++ " sind korrekt?")
      ]
    exhaustivenessCheck =  when isCorrect $ yesNo (cs ==  valid) $ multiLang [
      (English, "Given " ++ localise English what ++ " are exhaustive?"),
      (German, "Die angegebenen " ++ localise German what ++ " sind vollständig?")
      ]
    valid = M.keys $ M.filter id solution

printSolutionAndAssert
  :: OutputMonad m
  => Maybe String
  -> Rational
  -> Rated m
printSolutionAndAssert msolutionString points =
  for_ msolutionString (\solutionString ->
    when (points /= 1) $ paragraph $ do
      translate $ do
        english "The correct solution is:"
        german "Die richtige Lösung ist:"
      code solutionString
      pure ()
    )
  *> if points >= 1 % 2
    then pure (Just points)
    else do
      refuse $ pure ()
      pure Nothing

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
singleChoice what msolutionString solution choice = do
  checkCorrect
  *> printSolutionAndAssert msolutionString points
  where
    correct = solution == choice
    points = if correct then 1 else 0
    assert = continueOrAbort $ isJust msolutionString
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
multiLang = translated . M.fromList

localise :: Language -> Map Language String -> String
localise l lm = fromMaybe nonExistent $ M.lookup l lm
  where
    nonExistent
      | null lm   = error "missing translation"
      | otherwise = snd $ M.findMin lm

translate :: OutputMonad m => State (Map Language String) a -> LangM m
translate = translated . translations

translations :: State (Map k a1) a2 -> Map k a1
translations = flip execState M.empty

english :: String -> State (Map Language String) ()
english = modify . M.insertWith (flip (++)) English

german :: String -> State (Map Language String) ()
german = modify . M.insertWith (flip (++)) German

newtype GenericLangM m a = LangM { unLangM :: m a }
  deriving (Applicative, Functor)

type LangM m = LangM' m ()
type Rated m = LangM' m (Maybe Rational)

type LangLM l m a = GenericLangM (LanguageMonad l m) a
type LangM' m a = LangLM Language m a

instance MonadTrans GenericLangM where
  lift = LangM

enumerate
  :: OutputMonad m
  => (k -> String)
  -> (a -> String)
  -> Map k a
  -> LangM m
enumerate f g m = enumerateM (text . f) (M.toList $ text . g <$> m)

class GenericOutputMonad Language m => OutputMonad m where

class (Ord l, Monad m, Monad (LanguageMonad l m)) => GenericOutputMonad l m where
  -- | the monad handling multilingual input
  type LanguageMonad l m = (lm :: * -> *) | lm -> m l
  type Output l m
  type Result l m a
  -- | for assertions, i.e. expected behaviour is explanation
  -- (and abortion on 'False')
  assertion  :: Bool -> LangLM l m () -> LangLM l m ()
  -- | for printing a single image from file
  image      :: FilePath -> LangLM l m ()
  -- | for printing multiple images using the given map
  images     :: (k -> String) -> (a -> FilePath) -> Map k a -> LangLM l m ()
  -- | for a complete paragraph
  paragraph  :: LangLM l m () -> LangLM l m ()
  -- | should abort at once
  refuse     :: LangLM l m () -> LangLM l m ()
  -- | for displaying text
  text       :: String -> LangLM l m ()
  -- | for an enumerated sequence of elements
  enumerateM :: (a -> LangLM l m ()) -> [(a, LangLM l m ())] -> LangLM l m ()
  -- | for an unenumerated sequence of elements
  itemizeM   :: [LangLM l m ()] -> LangLM l m ()
  -- | for indentation
  indent     :: LangLM l m () -> LangLM l m ()
  -- | for LaTeX-Math code (i.e. without surrounding @$@)
  latex      :: String -> LangLM l m ()
  -- | for fixed width fonts (i.e. typewriter style)
  code       :: String -> LangLM l m ()
  -- | for language dependent formatting
  translated :: Map l String -> LangLM l m ()
  runLangM :: LangLM l m a -> m (Result l m a, Map l (Output l m))
  localised
    :: (String -> LangLM l m a)
    -> Map l String
    -> LangLM l m (Map l a)
  localised = defaultLocalised
  localised_
    :: (String -> LangLM l m a)
    -> Map l String
    -> LangLM l m ()
  localised_ = traverse_

defaultLocalised
  :: (Eq l, Monad m)
  => (String -> GenericLangM m a)
  -> Map l String
  -> GenericLangM m (Map l a)
defaultLocalised f ts = do
  let (keys, vals) = unzip $ M.toAscList ts
  vals' <- traverse f vals
  return $ M.fromAscList $ zip keys vals'

recoverFrom
  :: Alternative (LanguageMonad l m)
  => GenericLangM (LanguageMonad l m) ()
  -> GenericLangM (LanguageMonad l m) ()
recoverFrom x = LangM $ unLangM x <|> pure ()

recoverWith
  :: Alternative (LanguageMonad Language m)
  => a
  -> LangM' m b
  -> LangM' m (Either a b)
recoverWith x m = LangM $ (Right <$> unLangM m) <|> pure (Left x)

combineLangMs
  :: ([GenericReportT l o m a] -> GenericReportT l o m b)
  -> [GenericLangM (GenericReportT l o m) a]
  -> GenericLangM (GenericReportT l o m) b
combineLangMs f oms = LangM $ f $ map unLangM oms

combineLangM
  :: (GenericReportT l o m a -> GenericReportT l o m b -> GenericReportT l o m c)
  -> GenericLangM (GenericReportT l o m) a
  -> GenericLangM (GenericReportT l o m) b
  -> GenericLangM (GenericReportT l o m) c
combineLangM f x y = LangM $ f (unLangM x) (unLangM y)

combineReports
  :: Monad m
  => (Map l String -> o)
  -> ([[o]] -> o)
  -> [GenericLangM (GenericReportT l o m) a]
  -> GenericLangM (GenericReportT l o m) ()
combineReports l f = combineLangMs (Report.combineReports l f)

alignOutput
  :: Monad m
  => (Map l String -> o)
  -> ([o] -> o)
  -> GenericLangM (GenericReportT l o m) a
  -> GenericLangM (GenericReportT l o m) ()
alignOutput l = mapLangM . Report.alignOutput l

combineTwoReports
  :: Monad m
  => (Map l String -> o)
  -> ([o] -> [o] -> o)
  -> GenericLangM (GenericReportT l o m) a
  -> GenericLangM (GenericReportT l o m) b
  -> GenericLangM (GenericReportT l o m) ()
combineTwoReports l = combineLangM . Report.combineTwoReports l

out :: Monad m => GenericOut l o -> GenericLangM (GenericReportT l o m) ()
out = lift . Report . tell . (:[])

format :: Monad m => o -> GenericLangM (GenericReportT l o m) ()
format = out . Format

abortWith :: Monad m => o -> GenericLangM (GenericReportT l o m) b
abortWith d = (*>)
  (format d)
  $ lift $ Report $ MaybeT (return Nothing)

toAbort
  :: Monad m
  => (Map l String -> o)
  -> GenericLangM (GenericReportT l o m) a
  -> GenericLangM (GenericReportT l o m) b
toAbort l = mapLangM $ Report.toAbort l

mapLangM :: (m a -> m b) -> GenericLangM m a -> GenericLangM m b
mapLangM f om = LangM $ f $ unLangM om

instance GenericOutputMonad Language Maybe where
  type LanguageMonad Language Maybe = Maybe
  type Output Language Maybe = ()
  type Result Language Maybe a = ()
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
  translated _    = pure ()
  runLangM        = Just
    . (, foldr (`M.insert` ()) M.empty [minBound .. maxBound])
    . maybe (error "aborted") (const ()) . unLangM

data OutputException = Refused | AssertionFailed deriving Show
instance Exception OutputException

instance (Bounded l, Enum l, Ord l) => GenericOutputMonad l IO where
  type LanguageMonad l IO = GenericReportT l (IO ()) IO
  type Output l IO = IO ()
  type Result l IO a = Maybe a
  assertion b m = unless b $ m *> format (putStrLn "" >> throwIO AssertionFailed)
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
  refuse xs     = do
    xs
    indent $ text "No"
    format $ throwIO Refused
    pure ()
  latex         = format . putStrLn . ("LaTeX: " ++)
  code          = format . putStr . (\xs -> " <" ++ xs ++ "> ")
  translated lm = out (Localised lm)
  runLangM l = do
    (r, os) <- getOutsWithResult $ unLangM l
    let output = either id id $ foldr toOutput' (Right M.empty) os
    return (r, output)
    where
      toOutput' x xs = do
        xs' <- xs
        case x of
          Abort -> Left xs'
          Format o -> return $ insertAllWith (>>) o xs'
          Localised m -> return $ M.unionWith (>>) xs' (putStrLn <$> m)

insertAllWith
  :: (Bounded k, Enum k, Ord k)
  => (a -> a -> a)
  -> a
  -> Map k a
  -> Map k a
insertAllWith f x m =
  foldr (\k -> M.insertWith (flip f) k x) m [minBound .. maxBound]

newtype IsolatedOutput m a = IsolatedOutput { runOutput :: m a }
  deriving (Applicative, Functor, Monad) -- OutputMonad

runIsolated :: forall m. (MonadIO m, MonadCatch m) => IsolatedOutput m () -> m ()
runIsolated = flip catch handleExeption . runOutput
  where
    handleExeption :: OutputException -> m ()
    handleExeption = liftIO . (putStrLn . displayException)
