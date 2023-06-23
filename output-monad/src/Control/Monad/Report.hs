{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- | This module provides common skeletons for printing tasks
module Control.Monad.Report (
  GenericOut (..),
  GenericReportT (..),
  Out,
  ReportT,
  Report,
  alignOutput,
  combineReports,
  combineTwoReports,
  getAllOuts,
  getOutsWithResult,
  toAbort,
  toOutput,
  Language (..),
  ) where

import Control.Applicative              (Alternative)
import Control.Monad                    (void)
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Identity           (Identity)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer (
  MonadWriter (pass, tell),
  WriterT (WriterT, runWriterT),
  execWriterT,
  )
import Data.Map                         (Map)

data Language = English | German
  deriving (Bounded, Enum, Eq, Ord)

data GenericOut l o =
  Abort |
  Format o |
  Localised (Map l String)

newtype GenericReportT l o m r = Report { unReport :: MaybeT (WriterT [GenericOut l o] m) r }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadIO)

type ReportT = GenericReportT Language
type Out = GenericOut Language

instance MonadTrans (GenericReportT l o) where
  lift m = Report $ MaybeT $ WriterT $ fmap (\x -> (Just x, [])) m

type Report o r = ReportT o Identity r

getOutsWithResult :: Monad m => GenericReportT l o m a -> m (Maybe a, [GenericOut l o])
getOutsWithResult = runWriterT . getAllOuts'

getAllOuts :: Monad m => GenericReportT l o m a -> m [GenericOut l o]
getAllOuts = execWriterT . getAllOuts'

getAllOuts' :: Monad m => GenericReportT l o m a -> WriterT [GenericOut l o] m (Maybe a)
getAllOuts' r = do
  x <- runMaybeT $ unReport r
  case x of
    Nothing -> pass $ return (x, (Abort:))
    Just _ -> return x

combineReports
  :: Monad m
  => (Map l String -> o)
  -> ([[o]] -> o)
  -> [GenericReportT l o m a]
  -> GenericReportT l o m ()
combineReports l f rs = Report $ do
  rs' <- lift . lift $ getAllOuts `mapM` rs
  os <- MaybeT . return $ mapM (toOutput l) `mapM` rs'
  tell . (:[]) . Format $ f os

alignOutput
  :: Monad m
  => (Map l String -> o)
  -> ([o] -> o)
  -> GenericReportT l o m a
  -> GenericReportT l o m ()
alignOutput l f r = Report $ do
  r' <- lift . lift . getAllOuts $ r
  xs  <- MaybeT . return $ toOutput l `mapM` r'
  tell . (:[]) . Format $ f xs

toOutput
  :: (Map l String -> o)
  -> GenericOut l o
  -> Maybe o
toOutput _ Abort         = Nothing
toOutput _ (Format x)    = Just x
toOutput f (Localised x) = Just $ f x

combineTwoReports
  :: Monad m
  => (Map l String -> o)
  -> ([o] -> [o] -> o)
  -> GenericReportT l o m a
  -> GenericReportT l o m b
  -> GenericReportT l o m ()
combineTwoReports l f r1 r2 = do
  r1' <- lift $ getAllOuts r1
  case toOutput l `mapM` r1' of
    Nothing -> void r1
    Just x  -> alignOutput l (f x) r2

toAbort
  :: Monad m
  => (Map l String -> o)
  -> GenericReportT l o m a
  -> GenericReportT l o m b
toAbort l r = Report $ do
  r' <- lift . lift $ getAllOuts r
  xs  <- MaybeT . return $ toOutput l `mapM` r'
  tell $ Format <$> xs
  MaybeT $ return Nothing
