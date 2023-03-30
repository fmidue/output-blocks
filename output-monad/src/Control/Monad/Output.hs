{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provides common skeletons for printing tasks
module Control.Monad.Output (
  -- * Report monad
  Language (..),
  Out (..),
  ReportT (..),
  Report,
  getAllOuts,
  getOutsWithResult,
  toOutput,
  -- * Monad for translations
  LangM' (LangM, withLang),
  LangM,
  Rated,
  -- * Output monad
  OutputMonad (..),
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
  Language (..),
  Out (..),
  ReportT (..),
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
  paragraph $ indent $ localised code $ translations $
      if p
      then do
        english "Yes."
        german "Ja."
      else do
        english "No."
        german "Nein."
  pure ()

localised :: (String -> LangM' m a) -> Map Language String -> LangM' m a
localised f ts = LangM $ \l ->
  let t = localise l ts
  in f t `withLang` l

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

newtype LangM' m a = LangM { withLang :: Language -> m a}
type LangM m = LangM' m ()
type Rated m = LangM' m (Maybe Rational)

instance Functor m => Functor (LangM' m) where
  fmap f (LangM o) = LangM $ fmap f . o

instance Applicative m => Applicative (LangM' m) where
  pure x = LangM . const $ pure x
  LangM f <*> LangM x = LangM $ \l -> f l <*> x l

instance MonadTrans LangM' where
  lift m = LangM $ const m

enumerate
  :: OutputMonad m
  => (k -> String)
  -> (a -> String)
  -> Map k a
  -> LangM m
enumerate f g m = enumerateM (text . f) (M.toList $ text . g <$> m)

class Monad m => OutputMonad m where
  -- | for assertions, i.e. expected behaviour is explanation
  -- (and abortion on 'False')
  assertion  :: Bool -> LangM m -> LangM m
  -- | for printing a single image from file
  image      :: FilePath -> LangM m
  -- | for printing multiple images using the given map
  images     :: (k -> String) -> (a -> FilePath) -> Map k a -> LangM m
  -- | for a complete paragraph
  paragraph  :: LangM m -> LangM m
  -- | should abort at once
  refuse     :: LangM m -> LangM m
  -- | for displaying text
  text       :: String -> LangM m
  -- | for an enumerated sequence of elements
  enumerateM :: (a -> LangM m) -> [(a, LangM m)] -> LangM m
  -- | for an unenumerated sequence of elements
  itemizeM   :: [LangM m] -> LangM m
  -- | for indentation
  indent     :: LangM m -> LangM m
  -- | for LaTeX-Math code (i.e. without surrounding @$@)
  latex      :: String -> LangM m
  -- | for fixed width fonts (i.e. typewriter style)
  code       :: String -> LangM m
  -- | for language dependent formatting
  translated :: Map Language String -> LangM m

recoverFrom :: Alternative m => LangM m -> LangM m
recoverFrom x = LangM $ \l -> (x `withLang` l) <|> pure ()

recoverWith :: Alternative m => a -> LangM' m b -> LangM' m (Either a b)
recoverWith x m = LangM $ \l -> (Right <$> (m `withLang` l)) <|> pure (Left x)

combineLangMs :: ([m a] -> m b) -> [LangM' m a] -> LangM' m b
combineLangMs f oms = LangM $ \l ->
  let rs = (`withLang` l) <$> oms
  in f rs

combineLangM :: (m a -> m b -> m c) -> LangM' m a -> LangM' m b -> LangM' m c
combineLangM f x y = LangM $ \l ->
  let x' = x `withLang` l
      y' = y `withLang` l
  in f x' y'

combineReports
  :: Monad m
  => (Map Language String -> o)
  -> ([[o]] -> o)
  -> [LangM' (ReportT o m) a]
  -> LangM' (ReportT o m) ()
combineReports l = combineLangMs . Report.combineReports l

alignOutput
  :: Monad m
  => (Map Language String -> o)
  -> ([o] -> o)
  -> LangM' (ReportT o m) a
  -> LangM' (ReportT o m) ()
alignOutput l = mapLangM . Report.alignOutput l

combineTwoReports
  :: Monad m
  => (Map Language String -> o)
  -> ([o] -> [o] -> o)
  -> LangM' (ReportT o m) a
  -> LangM' (ReportT o m) b
  -> LangM' (ReportT o m) ()
combineTwoReports l = combineLangM . Report.combineTwoReports l

format :: Monad m => o -> LangM' (ReportT o m) ()
format = lift . Report . tell . (:[]) . Format

abortWith :: Monad m => o -> LangM' (ReportT o m) b
abortWith d = (*>)
  (format d)
  $ lift $ Report $ MaybeT (return Nothing)

toAbort
  :: Monad m
  => (Map Language String -> o)
  -> LangM' (ReportT o m) a
  -> LangM' (ReportT o m) b
toAbort l = mapLangM $ Report.toAbort l

mapLangM :: (m a -> m b) -> LangM' m a -> LangM' m b
mapLangM f om = LangM $ f . withLang om

instance OutputMonad Maybe where
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

data OutputException = Refused | AssertionFailed deriving Show
instance Exception OutputException

instance OutputMonad IO where
  assertion b m = unless b $ m *> lift (putStrLn "" >> throwIO AssertionFailed)
  image         = lift . putStr . ("file: " ++)
  images g f    = lift . putStrLn . foldrWithKey
    (\k x rs -> g k ++ ". file: " ++ f x ++ '\n' : rs)
    ""
  paragraph     = (*> lift (putStrLn ""))
  text          = lift . putStr
  enumerateM p  = foldl
    (\o (x, e) -> paragraph $ do o; p x; lift $ putStr "  "; e; pure ())
    (pure ())
  itemizeM      = foldl
    (\o x -> paragraph $ do o; lift $ putStr " -  "; x; pure ())
    (pure ())
  indent xs     = do
    lift $ putStr ">>>>"
    xs
    lift $ putStrLn "<<<<"
    pure ()
  refuse xs     = do
    xs
    indent $ text "No"
    lift $ throwIO Refused
    pure ()
  latex         = lift . putStrLn . ("LaTeX: " ++)
  code          = lift . putStr . (\xs -> " <" ++ xs ++ "> ")
  translated lm = LangM $ \l ->
    (text . fromMaybe "" $ M.lookup l lm) `withLang` l

newtype IsolatedOutput m a = IsolatedOutput { runOutput :: m a }
  deriving (Functor,Applicative,Monad,OutputMonad)

runIsolated :: forall m. (MonadIO m, MonadCatch m) => IsolatedOutput m () -> m ()
runIsolated = flip catch handleExeption . runOutput
  where
    handleExeption :: OutputException -> m ()
    handleExeption = liftIO . (putStrLn . displayException)
