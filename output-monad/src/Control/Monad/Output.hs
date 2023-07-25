{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  RunnableOutputMonad (..),
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
  ($=<<),
  ($>>),
  ($>>=),
  evalLangM,
  execLangM,
  multipleChoice,
  printSolutionAndAssert,
  runLangMReport,
  runLangMReportMultiLang,
  singleChoice,
  singleChoiceSyntax,
  withLang,
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
import Data.Foldable                    (foldl', for_, sequenceA_, traverse_)
import Data.Functor.Identity            (Identity (Identity))
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
printSolutionAndAssert msolutionString points = do
  for_ msolutionString (\solutionString ->
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
multiLang xs = translated $ \l ->
  fromMaybe "" $ lookup l xs

localise :: Language -> Map Language String -> String
localise l lm = fromMaybe nonExistent $ M.lookup l lm
  where
    nonExistent
      | null lm   = error "missing translation"
      | otherwise = snd $ M.findMin lm

translate :: OutputMonad m => State (Map Language String) a -> LangM m
translate xs = translated $ \l ->
  fromMaybe "" $ M.lookup l $ translations xs

translations :: State (Map k a1) a2 -> Map k a1
translations = flip execState M.empty

english :: String -> State (Map Language String) ()
english = modify . M.insertWith (flip (++)) English

german :: String -> State (Map Language String) ()
german = modify . M.insertWith (flip (++)) German

newtype GenericLangM l m a = LangM { unLangM :: m a }
  deriving (Applicative, Functor)

type LangM' m a = GenericLangM Language m a
type LangM m = LangM' m ()
type Rated m = LangM' m Rational

instance MonadTrans (GenericLangM l) where
  lift = LangM

enumerate
  :: OutputMonad m
  => (k -> String)
  -> (a -> String)
  -> Map k a
  -> LangM m
enumerate f g m = enumerateM (text . f) (M.toList $ text . g <$> m)

type OutputMonad m = GenericOutputMonad Language m

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
  translatedCode :: (l -> String) -> GenericLangM l m ()
  -- | for language dependent formatting
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
  => GenericLangM l m ()
  -> GenericLangM l m a
  -> GenericLangM l m a
x $>> y = LangM $ unLangM x >> unLangM y

class (GenericOutputMonad l m, Monad (RunMonad l m))
  => RunnableOutputMonad l m where
  -- | the monad handling multilingual output
  type RunMonad l m :: * -> *
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
  -> LangM' m b
  -> LangM' m (Either a b)
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
alignOutput = mapLangM . Report.alignOutput

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

abortWith :: Monad m => o -> GenericLangM l (GenericReportT l o m) b
abortWith d = (*>)
  (format d)
  $ lift $ Report $ MaybeT (return Nothing)

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

data OutputException = Refused | AssertionFailed deriving Show
instance Exception OutputException

instance (l ~ Language)
  => GenericOutputMonad l (GenericReportT l (IO ()) IO)
  where
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
    indent $ translate $ do
      english "No"
      german "Nein"
    format $ throwIO Refused
    pure ()
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

runLangMReport
  :: Monad m
  => o
  -> (o -> o -> o)
  -> GenericLangM l (GenericReportT l o m) a
  -> m (Maybe a, l -> o)
runLangMReport neutral f lm = do
  (r, os) <- getOutsWithResult $ unLangM lm
  let output l = either id id $ foldl' (toOutput' l) (Right neutral) os
  return (r, output)
  where
    toOutput' l xs x = do
      xs' <- xs
      case x of
        Abort -> Left xs'
        Format o -> Right $ f xs' o
        Localised m -> Right $ f xs' (m l)

runLangMReportMultiLang
  :: Monad m
  => o
  -> (o -> o -> o)
  -> ((l -> o) -> o)
  -> GenericLangM l (GenericReportT l o m) a
  -> m (Maybe a, o)
runLangMReportMultiLang neutral f toO lm = do
  (r, os) <- getOutsWithResult $ unLangM lm
  let output = either id id $ foldl' toOutput' (Right neutral) os
  return (r, output)
  where
    toOutput' xs x = do
      xs' <- xs
      case x of
        Abort -> Left xs'
        Format o -> Right $ f xs' o
        Localised m -> Right $ f xs' $ toO m

newtype IsolatedOutput m a = IsolatedOutput { runOutput :: m a }
  deriving (Applicative, Functor, Monad)

deriving instance GenericOutputMonad l m
  => GenericOutputMonad l (IsolatedOutput m)

runIsolated :: forall m. (MonadIO m, MonadCatch m) => IsolatedOutput m () -> m ()
runIsolated = flip catch handleExeption . runOutput
  where
    handleExeption :: OutputException -> m ()
    handleExeption = liftIO . (putStrLn . displayException)
