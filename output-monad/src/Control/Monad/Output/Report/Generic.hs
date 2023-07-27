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
import Control.Monad                    (void)
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer (
  MonadWriter (pass, tell),
  WriterT (WriterT, runWriterT),
  execWriterT,
  )
import Data.List.HT                     (takeUntil)
import Data.Maybe                       (catMaybes, isNothing)

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

untilNothing :: [Maybe a] -> [a]
untilNothing = catMaybes . takeUntil isNothing

getAllOuts'
  :: Monad m
  => GenericReportT l o m a
  -> WriterT [GenericOut l o] m (Maybe a)
getAllOuts' r = do
  x <- runMaybeT $ unReport r
  case x of
    Nothing -> pass $ return (x, (Abort :))
    Just _ -> return x

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
  let maybeOs = map (map toOutput . snd) rs'
      os = map untilNothing $ takeUntil (any isNothing) maybeOs
  tell . (:[]) . Localised $ \l -> f $ map (map ($ l)) os
  MaybeT . return $ mapM_ sequence_ maybeOs
  MaybeT . return $ mapM_ fst rs'

alignOutput
  :: Monad m
  => ([o] -> o)
  -> GenericReportT l o m a
  -> GenericReportT l o m a
alignOutput f r = Report $ do
  (result, r') <- lift . lift . getOutsWithResult $ r
  let maybeXs = map toOutput r'
      xs = untilNothing maybeXs
  tell . (:[]) . Localised $ \l -> f $ map ($ l) xs
  MaybeT . return $ sequence_ maybeXs
  MaybeT . return $ result

toOutput
  :: GenericOut l o
  -> Maybe (l -> o)
toOutput Abort         = Nothing
toOutput (Format x)    = Just $ const x
toOutput (Localised x) = Just x

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
  let maybeO1 = map toOutput r1'
      o1 = untilNothing maybeO1
  (result2, r2') <- lift . lift $ getOutsWithResult r2
  let maybeO2 = map toOutput r2'
      o2
        | any isNothing maybeO1 = []
        | otherwise = untilNothing maybeO2
  tell . (:[]) . Localised $ \l -> f (map ($ l) o1) $ map ($ l) o2
  MaybeT . return $ sequence_ maybeO1
  MaybeT . return $ sequence_ maybeO2
  void . MaybeT . return $ result1
  void . MaybeT . return $ result2

toAbort
  :: Monad m
  => GenericReportT l o m a
  -> GenericReportT l o m b
toAbort r = r >> Report (MaybeT $ pure Nothing)
