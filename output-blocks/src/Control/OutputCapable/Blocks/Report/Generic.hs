{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{- |
This module provides a generic output type for multilingual output as well as
a writer monad transformer which has the ability to abort execution.
-}
module Control.OutputCapable.Blocks.Report.Generic (
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
import Control.Monad                    (void)
import Control.Monad.Catch              (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer (
  MonadWriter (tell),
  WriterT (WriterT, runWriterT),
  execWriterT,
  )
import Data.Bifunctor                   (second)
import Data.List.HT                     (takeUntil)
import Data.Maybe                       (isNothing)

data GenericOut l o =
  Format o |
  Localised (l -> o)
  deriving Functor

newtype GenericReportT l o m r =
  Report { unReport :: MaybeT (WriterT [GenericOut l o] m) r }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadIO)

instance MonadTrans (GenericReportT l o) where
  lift m = Report $ MaybeT $ WriterT $ fmap (\x -> (Just x, [])) m

instance MonadThrow m => MonadThrow (GenericReportT l o m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (GenericReportT l o m) where
  catch x f = Report $ MaybeT $ WriterT $ catch
    (runWriterT . runMaybeT . unReport $ x)
    (runWriterT . runMaybeT . unReport . f)

getOutsWithResult
  :: GenericReportT l o m a
  -> m (Maybe a, [GenericOut l o])
getOutsWithResult = runWriterT . getAllOuts'

getAllOuts :: Monad m => GenericReportT l o m a -> m [GenericOut l o]
getAllOuts = execWriterT . getAllOuts'

getAllOuts'
  :: GenericReportT l o m a
  -> WriterT [GenericOut l o] m (Maybe a)
getAllOuts' r = runMaybeT $ unReport r

{-|
Combines the output of the list of given reports using the provided function.
Output is provided for all reports up to including the first failing report.
-}
combineReports
  :: Monad m
  => ([[o]] -> o)
  -> [GenericReportT l o m a]
  -> GenericReportT l o m ()
combineReports f rs = Report $ do
  rs' <- lift . lift $ getOutsWithResult `mapM` rs
  let maybeOs = map (second (map toOutput)) rs'
      os = map snd $ takeUntil (isNothing . fst) maybeOs
  tell . (:[]) . Localised $ \l -> f $ map (map ($ l)) os
  MaybeT . return $ mapM_ fst rs'

alignOutput
  :: Monad m
  => ([o] -> o)
  -> GenericReportT l o m a
  -> GenericReportT l o m a
alignOutput f r = Report $ do
  (result, r') <- lift . lift . getOutsWithResult $ r
  let xs = map toOutput r'
  tell . (:[]) . Localised $ \l -> f $ map ($ l) xs
  MaybeT . return $ result

toOutput
  :: GenericOut l o
  -> (l -> o)
toOutput (Format x)    = const x
toOutput (Localised x) = x

{-|
Combines the output of the two given reports using the provided functions.

If the execution aborts on the first report the second report is treated
as if has not produced any output.
-}
combineTwoReports
  :: Monad m
  => ([o] -> [o] -> o)
  -> GenericReportT l o m a
  -> GenericReportT l o m b
  -> GenericReportT l o m ()
combineTwoReports f r1 r2 = Report $ do
  (result1, r1') <- lift . lift $ getOutsWithResult r1
  let o1 = map toOutput r1'
  (result2, r2') <- lift . lift $ getOutsWithResult r2
  let o2
        | isNothing result1 = []
        | otherwise = map toOutput r2'
  tell . (:[]) . Localised $ \l -> f (map ($ l) o1) $ map ($ l) o2
  void . MaybeT . return $ result1
  void . MaybeT . return $ result2

toAbort
  :: Monad m
  => GenericReportT l o m a
  -> GenericReportT l o m b
toAbort r = r >> Report (MaybeT $ pure Nothing)
