{- |
This module provides a basic 'Language' type and functions for using it
as well as to this 'Language' specified versions of the generic report
and output.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.OutputCapable.Blocks.Report (
  module GenericReport,
  GenericOut (..),
  Out,
  ReportT,
  Report,
  Language (..),
  alignOutput,
  combineReports,
  combineTwoReports,
  english,
  german,
  getAllOuts,
  localise,
  toAbort,
  translations,
  ) where

import qualified Control.OutputCapable.Blocks.Report.Generic as GenericReport (
  alignOutput,
  combineReports,
  combineTwoReports,
  getAllOuts,
  toAbort,
  )
import Control.OutputCapable.Blocks.Report.Generic as GenericReport (
  GenericOut (..),
  getOutsWithResult,
  toOutput,
  )
import Control.OutputCapable.Blocks.Report.Generic (GenericReportT (..))

import qualified Data.Map               as M

import Autolib.Hash                     (Hashable)
import Autolib.Reader                   (Reader)
import Autolib.ToDoc                    (ToDoc)
import Control.Monad.Identity           (Identity)
import Control.Monad.State              (State, execState, modify)
import Data.Data                        (Data)
import Data.Map                         (Map)
import Data.Maybe                       (fromMaybe)
import GHC.Generics                     (Generic)

data Language = English | German
  deriving (Bounded, Data, Enum, Eq, Generic, Hashable, Ord, Read, Reader, Show, ToDoc)

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

{-|
This is a more specific version of 'GenericReport.getAllOuts'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
getAllOuts :: Monad m => GenericReportT l o m () -> m [GenericOut l o]
getAllOuts = GenericReport.getAllOuts

{-|
This is a more specific version of 'GenericReport.combineReports'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
combineReports
  :: Monad m
  => ([[o]] -> o)
  -> [GenericReportT l o m ()]
  -> GenericReportT l o m ()
combineReports = GenericReport.combineReports

{-|
This is a more specific version of 'GenericReport.alignOutput'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
alignOutput
  :: Monad m
  => ([o] -> o)
  -> GenericReportT l o m ()
  -> GenericReportT l o m ()
alignOutput = GenericReport.alignOutput

{-|
This is a more specific version of 'GenericReport.combineTwoReports'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
combineTwoReports
  :: Monad m
  => ([o] -> [o] -> o)
  -> GenericReportT l o m ()
  -> GenericReportT l o m ()
  -> GenericReportT l o m ()
combineTwoReports = GenericReport.combineTwoReports

{-|
This is a more specific version of 'GenericReport.toAbort'
which enforces the usage pattern.
You should always prefer this version over the generic.
-}
toAbort
  :: Monad m
  => GenericReportT l o m ()
  -> GenericReportT l o m ()
toAbort = GenericReport.toAbort
