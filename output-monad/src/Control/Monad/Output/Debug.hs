{-# LANGUAGE GADTs #-}
-- | Provides common functions for running Output-Monad within IO

module Control.Monad.Output.Debug where

import Control.Monad.Output (
  GenericReportT,
  LangM',
  LangM,
  Language,
  Rated,
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
  :: (m ~ GenericReportT Language (IO ()) IO,
      mr ~ GenericReportT Language (IO Rational) IO,
      Show a)
  => Language
  -> IO inst
  -> (inst -> LangM m)
  -> (inst -> a -> LangM m)
  -> (inst -> a -> Rated mr)
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
  completeRes <- runRated language (full inst value)
  print completeRes

checkConfigWith
  :: (m ~ GenericReportT Language (IO ()) IO)
  => Language
  -> config
  -> (config -> LangM m)
  -> IO Bool
checkConfigWith language conf check = isJust <$> run language (check conf)

runDefault
  :: (m ~ GenericReportT Language (IO a) IO)
  => a
  -> Language
  -> LangM' m a
  -> IO (Maybe a)
runDefault x language thing = do
  (r, sayThing) <- runLangMReport (pure x) (>>) thing
  void $ sayThing language
  pure r

run
  :: (m ~ GenericReportT Language (IO ()) IO)
  => Language
  -> LangM m
  -> IO (Maybe ())
run = runDefault ()

runRated
  :: (m ~ GenericReportT Language (IO Rational) IO)
  => Language
  -> Rated m
  -> IO (Maybe Rational)
runRated = runDefault 0
