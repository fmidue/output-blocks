{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{- |
This module provides a generic output type for multilingual output as well as
a writer monad transformer which has the ability to abort execution.
-}
module Control.Monad.Output.Report.Generic (
  GenericOut (..),
  GenericReportT (..),
  alignOutput,
  combineReports,
  combineTwoReports,
  getAllOuts,
  getOutsWithResult,
  toAbort,
  toOutput,
  ) where

import Control.Applicative              (Alternative)
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer (
  MonadWriter (pass, tell),
  WriterT (WriterT, runWriterT),
  execWriterT,
  )

data GenericOut l o =
  Abort |
  Format o |
  Localised (l -> o)
  deriving Functor

newtype GenericReportT l o m r =
  Report { unReport :: MaybeT (WriterT [GenericOut l o] m) r }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadIO)

instance MonadTrans (GenericReportT l o) where
  lift m = Report $ MaybeT $ WriterT $ fmap (\x -> (Just x, [])) m

getOutsWithResult
  :: Monad m
  => GenericReportT l o m a
  -> m (Maybe a, [GenericOut l o])
getOutsWithResult = runWriterT . getAllOuts'

getAllOuts :: Monad m => GenericReportT l o m a -> m [GenericOut l o]
getAllOuts = execWriterT . getAllOuts'

getAllOuts'
  :: Monad m
  => GenericReportT l o m a
  -> WriterT [GenericOut l o] m (Maybe a)
getAllOuts' r = do
  x <- runMaybeT $ unReport r
  case x of
    Nothing -> pass $ return (x, (Abort:))
    Just _ -> return x

combineReports
  :: Monad m
  => ([[o]] -> o)
  -> [GenericReportT l o m a]
  -> GenericReportT l o m ()
combineReports f rs = Report $ do
  rs' <- lift . lift $ getAllOuts `mapM` rs
  os <- MaybeT . return $ mapM toOutput `mapM` rs'
  tell . (:[]) . Localised $ \l -> f $ map (map ($ l)) os

alignOutput
  :: Monad m
  => ([o] -> o)
  -> GenericReportT l o m a
  -> GenericReportT l o m ()
alignOutput f r = Report $ do
  r' <- lift . lift . getAllOuts $ r
  xs  <- MaybeT . return $ toOutput `mapM` r'
  tell . (:[]) . Localised $ \l -> f $ map ($ l) xs

toOutput
  :: GenericOut l o
  -> Maybe (l -> o)
toOutput Abort         = Nothing
toOutput (Format x)    = Just $ const x
toOutput (Localised x) = Just x

combineTwoReports
  :: Monad m
  => ([o] -> [o] -> o)
  -> GenericReportT l o m a
  -> GenericReportT l o m b
  -> GenericReportT l o m ()
combineTwoReports f r1 r2 = Report $ do
  r1' <- lift . lift $ getAllOuts r1
  o1 <- MaybeT . return $ toOutput `mapM` r1'
  r2' <- lift . lift $ getAllOuts r2
  o2 <- MaybeT . return $ toOutput `mapM` r2'
  tell . (:[]) . Localised $ \l -> f (map ($ l) o1) $ map ($ l) o2

toAbort
  :: Monad m
  => GenericReportT l o m a
  -> GenericReportT l o m b
toAbort r = Report $ do
  r' <- lift . lift $ getAllOuts r
  xs  <- MaybeT . return $ toOutput `mapM` r'
  tell $ Localised <$> xs
  MaybeT $ return Nothing
