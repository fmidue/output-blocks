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
