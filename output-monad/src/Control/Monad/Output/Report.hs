{- |
This module provides a basic 'Language' type and functions for using it
as well as to this 'Language' specified versions of the generic report
and output.
-}
module Control.Monad.Output.Report (
  module GenericReport,
  GenericOut (..),
  Out,
  ReportT,
  Report,
  Language (..),
  english,
  german,
  localise,
  translations,
  ) where

import Control.Monad.Output.Report.Generic as GenericReport (
  GenericOut (..),
  alignOutput,
  combineReports,
  combineTwoReports,
  getAllOuts,
  getOutsWithResult,
  toAbort,
  toOutput,
  )
import Control.Monad.Output.Report.Generic (GenericReportT (..))

import qualified Data.Map               as M

import Control.Monad.Identity           (Identity)
import Control.Monad.State              (State, execState, modify)
import Data.Map                         (Map)
import Data.Maybe                       (fromMaybe)

data Language = English | German
  deriving (Bounded, Enum, Eq, Ord)

type ReportT = GenericReportT Language
type Out = GenericOut Language

type Report o r = ReportT o Identity r

translations :: State (Map k a1) a2 -> Map k a1
translations = flip execState M.empty

english :: String -> State (Map Language String) ()
english = modify . M.insertWith (flip (++)) English

german :: String -> State (Map Language String) ()
german = modify . M.insertWith (flip (++)) German

localise :: Language -> Map Language String -> String
localise l lm = fromMaybe nonExistent $ M.lookup l lm
  where
    nonExistent
      | null lm   = error "missing translation"
      | otherwise = snd $ M.findMin lm
