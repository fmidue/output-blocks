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
  checkTranslations,
  foldrOutput,
  getSpecialOutputSequence,
  getSpecialOutputSequenceWithRating,
  specialToOutputCapable,
  withRefusal,
  -- ** other
  checkTranslation,
  ) where

import qualified Control.OutputCapable.Blocks.Generic.Type as Generic (
  foldrOutput,
  getOutputSequence,
  getOutputSequenceWithRating,
  inspectTranslations,
  toOutputCapable,
  withRefusal,
  )

import qualified Data.Map                         as M (keys)

import Control.OutputCapable.Blocks.Generic.Type (GenericOutput (..))
import Control.OutputCapable.Blocks (
  LangM,
  Language (English),
  OutputCapable,
  Rated,
  ReportT,
  )

import Data.List                        ((\\))
import Data.Map                         (Map)
import Data.Traversable                 (for)

-- | 'GenericOutput' but with translations fixed to 'Language'
type SpecialOutput = GenericOutput Language

-- | 'SpecialOutput' without 'Special' elements
type Output = SpecialOutput ()

{-|
Converts non graded 'OutputCapable' value using 'GenericOutput'
into a list of 'Output'
-}
getOutputSequence :: Functor m => LangM (ReportT Output m) -> m [Output]
getOutputSequence = Generic.getOutputSequence English

{-|
Converts graded 'OutputCapable' value using 'GenericOutput'
into a rating and a list of 'Output'
-}
getOutputSequenceWithRating
  :: Functor m
  => Rated (ReportT Output m)
  -> m (Maybe Rational, [Output])
getOutputSequenceWithRating = Generic.getOutputSequenceWithRating English

{- |
Convert a list of 'Output' into any instance of 'OutputCapable'
-}
toOutputCapable :: OutputCapable m => [Output] -> LangM m
toOutputCapable = Generic.toOutputCapable pure

{-|
Converts non graded 'OutputCapable' value using 'GenericOutput'
into a list of 'SpecialOutput'
-}
getSpecialOutputSequence
  :: Functor m
  => LangM (ReportT (SpecialOutput element) m)
  -> m [SpecialOutput element]
getSpecialOutputSequence = Generic.getOutputSequence English

{-|
Converts graded 'OutputCapable' value using 'GenericOutput'
into a rating and a list of 'SpecialOutput'
-}
getSpecialOutputSequenceWithRating
  :: Functor m
  => Rated (ReportT (SpecialOutput element) m)
  -> m (Maybe Rational, [SpecialOutput element])
getSpecialOutputSequenceWithRating = Generic.getOutputSequenceWithRating English

{- |
Convert a list of 'SpecialOutput' into any instance of 'OutputCapable'
-}
specialToOutputCapable
  :: OutputCapable m
  => (element -> LangM m)
  -> [SpecialOutput element]
  -> LangM m
specialToOutputCapable = Generic.toOutputCapable

{-|
A right fold with the possibility to inspect every node.
-}
foldrOutput
  :: (a -> a -> a)
  -> (SpecialOutput element -> a)
  -> SpecialOutput element
  -> a
foldrOutput = Generic.foldrOutput

{-|
Checks whether any refusal exists within the given 'GenericOutput'.
-}
withRefusal :: (element -> Bool) -> SpecialOutput element -> Bool
withRefusal = Generic.withRefusal

{-|
Checks a 'Map' for missing translations and reports those as list.
-}
checkTranslation :: Map Language String -> [String]
checkTranslation xs =
  let ls = [minBound ..] \\ M.keys xs
  in for ls $ \l -> "Missing " ++ show l ++ " translation for " ++ show xs ++ "."

{-|
Checks 'SpecialOutput' for missing translations.
-}
checkTranslations
  :: (element -> [String])
  -> SpecialOutput element
  -> [String]
checkTranslations inspectSpecial =
  Generic.inspectTranslations inspectSpecial checkTranslation (++) []
