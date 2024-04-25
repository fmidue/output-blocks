{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- | Provides common functions for running Output-Monad within IO

module Control.Monad.Output.Debug where

import qualified Data.Text                        as T (unpack)
import qualified Text.PrettyPrint.HughesPJClass   as HughesPJ (
  Pretty,
  pPrint,
  render,
  )
import qualified Text.PrettyPrint.Leijen.Text     as Leijen (
  Pretty,
  displayTStrict,
  pretty,
  renderPretty,
  )

import Control.Monad.Output (
  GenericReportT,
  LangM',
  LangM,
  Language,
  )
import Control.Monad.Output.Generic (
  runLangMReport,
  )

import Control.Monad                    (void, when)
import Control.Monad.Extra              (whenJust)
import Data.Maybe                       (isJust, isNothing)

showDescription
  :: (m ~ GenericReportT Language (IO ()) IO)
  => Language
  -> IO inst
  -> (inst -> LangM m)
  -> IO (Maybe ())
showDescription language generate f = do
  inst <- generate
  run language (f inst)

data Display a where
  Manual :: (a -> String) -> Display a
  AutoHughesPJ :: HughesPJ.Pretty a => Display a
  AutoLeijen :: Leijen.Pretty a => Display a

display :: Display a -> a -> String
display (Manual f) x = f x
display AutoHughesPJ x = HughesPJ.render $ HughesPJ.pPrint x
display AutoLeijen x = T.unpack
  . Leijen.displayTStrict
  . Leijen.renderPretty 0.4 80
  $ Leijen.pretty x

testTask
  :: (m ~ GenericReportT Language (IO ()) IO, Show a, Show b, Show c, Show d)
  => Maybe (Display a)
  -> Language
  -> IO inst
  -> (inst -> LangM' m b)
  -> (inst -> a -> LangM' m c)
  -> (inst -> a -> LangM' m d)
  -> IO a
  -> IO ()
testTask pretty language generate f partial full getSubmission = do
  inst <- generate
  desc <- run language (f inst)
  print desc
  value <- getSubmission
  putStrLn "---- Input ----"
  print value
  whenJust pretty $ \displayMethod -> do
    putStrLn "---- Prettified Input ----"
    putStrLn $ display displayMethod value
  putStrLn "---- Partial ----"
  partialRes <- run language (partial inst value)
  print partialRes
  when (isNothing partialRes)
    $ putStrLn "!!! The following would not be printed in Autotool !!!"
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
