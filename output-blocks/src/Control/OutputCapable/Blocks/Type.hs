{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-|
A version of the 'Control.OutputCapable.Blocks.Generic.Type' module
specialised to 'Language'.
It provides basically the same interface but specialised to 'Output'
and additionally a somewhat more general 'SpecialOutput' variant.
Both type synonyms are also provided.
-}
module Control.OutputCapable.Blocks.Type (
  -- * common constructors
  pattern Assertion,
  pattern Image,
  pattern Images,
  pattern Paragraph,
  pattern Refuse,
  pattern Enumerated,
  pattern Itemized,
  pattern Indented,
  pattern Latex,
  pattern Code,
  pattern Translated,
  pattern Special,
  -- * the interface
  -- ** for 'Output'
  type Output,
  getOutputSequence,
  getOutputSequenceWithRating,
  toOutputCapable,
  -- ** for 'SpecialOutput'
  type SpecialOutput,
  getSpecialOutputSequence,
  getSpecialOutputSequenceWithRating,
  specialToOutputCapable,
  ) where

import qualified Control.OutputCapable.Blocks.Generic.Type as Generic (
  getOutputSequence,
  getOutputSequenceWithRating,
  toOutputCapable,
  )

import Control.OutputCapable.Blocks.Generic.Type (GenericOutput (..))
import Control.OutputCapable.Blocks (
  LangM,
  Language (English),
  OutputCapable,
  Rated,
  ReportT,
  )

type SpecialOutput = GenericOutput Language
type Output = SpecialOutput ()

getOutputSequence :: Functor m => LangM (ReportT Output m) -> m [Output]
getOutputSequence = Generic.getOutputSequence English

getOutputSequenceWithRating
  :: Functor m
  => Rated (ReportT Output m)
  -> m (Maybe Rational, [Output])
getOutputSequenceWithRating = Generic.getOutputSequenceWithRating English

toOutputCapable :: OutputCapable m => [Output] -> LangM m
toOutputCapable = Generic.toOutputCapable pure

getSpecialOutputSequence
  :: Functor m
  => LangM (ReportT (SpecialOutput element) m)
  -> m [SpecialOutput element]
getSpecialOutputSequence = Generic.getOutputSequence English

getSpecialOutputSequenceWithRating
  :: Functor m
  => Rated (ReportT (SpecialOutput element) m)
  -> m (Maybe Rational, [SpecialOutput element])
getSpecialOutputSequenceWithRating = Generic.getOutputSequenceWithRating English

specialToOutputCapable
  :: OutputCapable m
  => (element -> LangM m)
  -> [SpecialOutput element]
  -> LangM m
specialToOutputCapable = Generic.toOutputCapable
