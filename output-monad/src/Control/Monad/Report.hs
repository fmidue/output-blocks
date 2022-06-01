{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- | This module provides common skeletons for printing tasks
module Control.Monad.Report (
  Out (..),
  ReportT (..),
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
  deriving (Enum, Eq, Ord)

data Out o =
  Abort |
  Format o |
  Localised (Map Language String)

newtype ReportT o m r = Report { unReport :: MaybeT (WriterT [Out o] m) r }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadIO)

instance MonadTrans (ReportT o) where
  lift m = Report $ MaybeT $ WriterT $ fmap (\x -> (Just x, [])) m

type Report o r = ReportT o Identity r

getOutsWithResult :: Monad m => ReportT o m a -> m (Maybe a, [Out o])
getOutsWithResult = runWriterT . getAllOuts'

getAllOuts :: Monad m => ReportT o m a -> m [Out o]
getAllOuts = execWriterT . getAllOuts'

getAllOuts' :: Monad m => ReportT o m a -> WriterT [Out o] m (Maybe a)
getAllOuts' r = do
  x <- runMaybeT $ unReport r
  case x of
    Nothing -> pass $ return (x, (Abort:))
    Just _ -> return x

combineReports
  :: Monad m
  => (Map Language String -> o)
  -> ([[o]] -> o)
  -> [ReportT o m a]
  -> ReportT o m ()
combineReports l f rs = Report $ do
  rs' <- lift . lift $ getAllOuts `mapM` rs
  os <- MaybeT . return $ mapM (toOutput l) `mapM` rs'
  tell . (:[]) . Format $ f os

alignOutput
  :: Monad m
  => (Map Language String -> o)
  -> ([o] -> o)
  -> ReportT o m a
  -> ReportT o m ()
alignOutput l f r = Report $ do
  r' <- lift . lift . getAllOuts $ r
  xs  <- MaybeT . return $ toOutput l `mapM` r'
  tell . (:[]) . Format $ f xs

toOutput
  :: (Map Language String -> o)
  -> Out o
  -> Maybe o
toOutput _ Abort         = Nothing
toOutput _ (Format x)    = Just x
toOutput f (Localised x) = Just $ f x

combineTwoReports
  :: Monad m
  => (Map Language String -> o)
  -> ([o] -> [o] -> o)
  -> ReportT o m a
  -> ReportT o m b
  -> ReportT o m ()
combineTwoReports l f r1 r2 = do
  r1' <- lift $ getAllOuts r1
  case toOutput l `mapM` r1' of
    Nothing -> void r1
    Just x  -> alignOutput l (f x) r2

toAbort
  :: Monad m
  => (Map Language String -> o)
  -> ReportT o m a
  -> ReportT o m b
toAbort l r = Report $ do
  r' <- lift . lift $ getAllOuts r
  xs  <- MaybeT . return $ toOutput l `mapM` r'
  tell $ Format <$> xs
  MaybeT $ return Nothing
