{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- | Provides common functions for running Output-Monad within IO

module Control.Monad.Output.Debug where

import Control.Monad.Output (
  GenericReportT,
  LangM',
  LangM,
  Language,
  )
import Control.Monad.Output.Generic (
  runLangMReport,
  )

import Control.Monad                    (void)
import Data.Maybe                       (isJust)

showDescription
  :: (m ~ GenericReportT Language (IO ()) IO)
  => Language
  -> IO inst
  -> (inst -> LangM m)
  -> IO (Maybe ())
showDescription language generate f = do
  inst <- generate
  run language (f inst)

testTask
  :: (m ~ GenericReportT Language (IO ()) IO, Show a, Show b, Show c, Show d)
  => Language
  -> IO inst
  -> (inst -> LangM' m b)
  -> (inst -> a -> LangM' m c)
  -> (inst -> a -> LangM' m d)
  -> IO a
  -> IO ()
testTask language generate f partial full getSubmission = do
  inst <- generate
  desc <- run language (f inst)
  print desc
  value <- getSubmission
  putStrLn "---- Input ----"
  print value
  putStrLn "---- Partial ----"
  partialRes <- run language (partial inst value)
  print partialRes
  putStrLn "---- Complete ----"
  completeRes <- run language (full inst value)
  print completeRes

checkConfigWith
  :: (m ~ GenericReportT Language (IO ()) IO)
  => Language
  -> config
  -> (config -> LangM m)
  -> IO Bool
checkConfigWith language conf check = isJust <$> run language (check conf)

run
  :: (m ~ GenericReportT Language (IO ()) IO)
  => Language
  -> LangM' m a
  -> IO (Maybe a)
run language thing = do
  (r, sayThing) <- runLangMReport (pure ()) (>>) thing
  void $ sayThing language
  pure r
