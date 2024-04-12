{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-|
A version of the 'Control.Monad.Output.Generic.Type' module specialised to
'Language'.
It provides basically the same interface but specialised to 'Output'
and additionally a somewhat more general 'SpecialOutput' variant.
Both type synonyms are also provided.
-}
module Control.Monad.Output.Type (
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
  toOutputMonad,
  -- ** for 'SpecialOutput'
  type SpecialOutput,
  getSpecialOutputSequence,
  getSpecialOutputSequenceWithRating,
  specialToOutputMonad,
  ) where

import qualified Control.Monad.Output.Generic.Type as Generic (
  getOutputSequence,
  getOutputSequenceWithRating,
  toOutputMonad,
  )

import Control.Monad.Output.Generic.Type (GenericOutput (..))
import Control.Monad.Output (LangM, Language, OutputMonad, Rated, ReportT)

type SpecialOutput = GenericOutput Language
type Output = SpecialOutput ()

getOutputSequence :: Monad m => LangM (ReportT Output m) -> m [Output]
getOutputSequence = Generic.getOutputSequence

getOutputSequenceWithRating
  :: Monad m
  => Rated (ReportT Output m)
  -> m (Maybe Rational, [Output])
getOutputSequenceWithRating = Generic.getOutputSequenceWithRating

toOutputMonad :: OutputMonad m => [Output] -> LangM m
toOutputMonad = Generic.toOutputMonad pure

getSpecialOutputSequence
  :: Monad m
  => LangM (ReportT (SpecialOutput element) m)
  -> m [SpecialOutput element]
getSpecialOutputSequence = Generic.getOutputSequence

getSpecialOutputSequenceWithRating
  :: Monad m
  => Rated (ReportT (SpecialOutput element) m)
  -> m (Maybe Rational, [SpecialOutput element])
getSpecialOutputSequenceWithRating = Generic.getOutputSequenceWithRating

specialToOutputMonad
  :: OutputMonad m
  => (element -> LangM m)
  -> [SpecialOutput element]
  -> LangM m
specialToOutputMonad = Generic.toOutputMonad
