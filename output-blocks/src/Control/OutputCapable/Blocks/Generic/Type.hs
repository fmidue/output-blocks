{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Provides an Algebraic Data Type to represent 'GenericOutput'.
This can be useful in cases where the raw representation needs to be persisted
or for converting the type of 'GenericOutputCapable'.
-}
module Control.OutputCapable.Blocks.Generic.Type
    ( GenericOutput (..)
    , foldMapOutputBy
    , getOutputSequence
    , getOutputSequenceWithResult
    , getOutputSequenceWithRating
    , inspectTranslations
    , toOutputCapable
    ) where

import Autolib.Hash                     (Hashable)
import Autolib.Reader                   (Reader)
import Autolib.ToDoc                    (ToDoc)
import Control.OutputCapable.Blocks.Generic (
  GenericLangM,
  GenericOutputCapable (..),
  GenericReportT,
  alignOutput,
  collapsed,
  combineReports,
  combineTwoReports,
  format,
  runLangMReportMultiLang,
  toAbort,
  translate,
  translateCode,
  )
import Control.Monad.Trans.State    (put)
import Data.Data                    (Data)
import Data.Foldable                (for_)
import Data.Map                     (Map)
import Data.Tuple.Extra             (dupe, both, first, second)
import GHC.Generics                 (Generic)

import qualified Data.Map as Map


{-|
Data type miming the OutputCapable class interface.
Can be used to Prototype without defining additional instances.
The result can be converted into any member of the class.
-}
data GenericOutput language element
    = YesNo Bool [GenericOutput language element]
    -- ^ output condition check, but continue afterwards
    | Image FilePath
    -- ^ a single image
    | Images (Map String FilePath)
    -- ^ multiple images with a text tag that is to be printed (e.g. a number)
    | Paragraph [GenericOutput language element]
    -- ^ to group (and separate) output
    | Enumerated [([GenericOutput language element], [GenericOutput language element])]
    -- ^ an enumeration with the enumerator first and content second
    | Itemized [[GenericOutput language element]]
    -- ^ like enumeration (but without enumerator)
    | Indented [GenericOutput language element]
    -- ^ for indenting output
    | Latex String
    -- ^ latex code (for formulas and text blocks only)
    | Folded Bool (Map language String) [GenericOutput language element]
    -- ^ minimisable output with default open-state, title and content
    | Code (Map language String)
    -- ^ to output as text with fixed width, providing translations
    | Translated (Map language String)
    -- ^ normal text with translations
    | Special element
    -- ^ allows abbreviating several complex parts
    --   which have special rendering functions
    --   which are not easily representable as ADT
  deriving (Data, Eq, Foldable, Functor, Generic, Hashable, Read, Reader, Show, ToDoc)

{-|
OutputCapable instances for 'GenericOutput',
allowing for free conversion between ADT and interface.
-}
instance
  (Bounded language, Enum language, Monad m, Ord language)
  => GenericOutputCapable language (
    GenericReportT language (GenericOutput language element) m
    )
  where
  assertion b = (if b then id else toAbort) . alignOutput (YesNo b)

  image = format . Image

  images descF fileF = format . Images . Map.mapKeys descF . Map.map fileF

  paragraph = alignOutput Paragraph

  refuse = toAbort

  enumerateM f tups = combineReports
                        (Enumerated . concatMap expose . concat)
                         combine
    where
      combine = uncurry (combineTwoReports . curry $ Enumerated . (:[]))
              . first f
            <$> tups

      expose (Enumerated list) = list
      expose _                 = error "This is impossible"

  itemizeM = combineReports Itemized

  indent = alignOutput Indented

  latex = format . Latex

  folded b t = alignOutput (Folded b $ toMap t)

  translatedCode =  format . Code . toMap

  translated = format . Translated . toMap



{- |
Convert a list of 'GenericOutput' into any member of 'GenericOutputCapable'
-}
toOutputCapable
  :: GenericOutputCapable language m
  => (element -> GenericLangM language m ())
  -> (Bool -> GenericLangM language m () -> GenericLangM language m ())
  -> [GenericOutput language element]
  -> GenericLangM language m ()
toOutputCapable toOutputPart yesNoDisplay parts = for_ parts toInterface
  where
    toInterface res = case res of
      YesNo b xs       -> yesNoDisplay b $ toOutputCapable' xs
      Image path       -> image path
      Images m         -> images id id m
      Paragraph xs     -> paragraph $ toOutputCapable' xs
      Enumerated list  -> enumerateM id $ map (both toOutputCapable') list
      Itemized xs      -> itemizeM $ map toOutputCapable' xs
      Indented xs      -> indent $ toOutputCapable' xs
      Latex s          -> latex s
      Folded b t c     -> collapsed b (put t) $ toOutputCapable' c
      Code m           -> translateCode $ put m
      Translated m     -> translate $ put m
      Special element  -> toOutputPart element
    toOutputCapable' = toOutputCapable toOutputPart yesNoDisplay



{-|
Converts non graded 'GenericOutputCapable' value using 'GenericOutput'
into a list of 'GenericOutput'
-}
getOutputSequence
  :: Functor m
  => language
  -> GenericLangM language (GenericReportT language (GenericOutput language element) m) ()
  -> m [GenericOutput language element]
getOutputSequence f = (snd <$>) . getOutputSequenceWithResult f



{-|
More concretely typed alias of 'getOutputSequenceWithResult'

Converts graded 'GenericOutputCapable' value using 'GenericOutput'
into a rating and a list of 'GenericOutput'
-}
getOutputSequenceWithRating
  :: Functor m
  => language
  -> GenericLangM language (GenericReportT language (GenericOutput language element) m) Rational
  -> m (Maybe Rational, [GenericOutput language element])
getOutputSequenceWithRating = getOutputSequenceWithResult


{-|
Converts 'GenericOutputCapable' value using 'GenericOutput'
into a result and a list of 'GenericOutput'

Consider using 'getOutputSequenceWithRating'
or even more specific versions of 'Control.OutputCapable.Blocks.Type'
in order to get better error messages on implementation errors.
-}
getOutputSequenceWithResult
  :: Functor m
  => language
  -> GenericLangM language (GenericReportT language (GenericOutput language element) m) a
  -> m (Maybe a, [GenericOutput language element])
getOutputSequenceWithResult language lm = second unbox <$>
    runLangMReportMultiLang (Paragraph []) gather ($ language) lm
  where
    gather (Paragraph xs) x = Paragraph (xs ++ [x])
    gather  _ _ = error "this is impossible"

    unbox (Paragraph xs) = xs
    unbox  _ = error "this is impossible"


{-|
A right fold with the possibility to inspect every node.

@since: 0.4
-}
foldMapOutputBy
  :: (a -> a -> a)
  -> (GenericOutput language element -> a)
  -> GenericOutput language element
  -> a
foldMapOutputBy f evaluate x = case x of
  YesNo _ xs        -> foldr (f . descend) (evaluate x) xs
  Image {}          -> evaluate x
  Images {}         -> evaluate x
  Paragraph xs      -> foldr (f . descend) (evaluate x) xs
  Enumerated xs   ->
    foldr
    (\next done -> uncurry f $ both (foldr (f . descend) done) next)
    (evaluate x)
    xs
  Itemized xs       -> foldr (flip (foldr (f . descend))) (evaluate x) xs
  Indented xs       -> foldr (f . descend) (evaluate x) xs
  Latex {}          -> evaluate x
  Folded _ _ xs     -> foldr (f . descend) (evaluate x) xs
  Code {}           -> evaluate x
  Translated {}     -> evaluate x
  Special {}        -> evaluate x
  where
    descend = foldMapOutputBy f evaluate

{-|
Inspects translations provided the given inspect and combining functions.

@since: 0.3.0.1
-}
inspectTranslations
  :: (element -> a)
  -> (Map language String -> a)
  -> (a -> a -> a)
  -> a
  -> GenericOutput language element
  -> a
inspectTranslations inspectSpecial inspectTranslation f z = foldMapOutputBy f $ \case
  YesNo {}          -> z
  Image {}          -> z
  Images {}         -> z
  Paragraph {}      -> z
  Enumerated {}     -> z
  Itemized {}       -> z
  Indented {}       -> z
  Latex {}          -> z
  Folded _ t _      -> inspectTranslation t
  Code ts           -> inspectTranslation ts
  Translated ts     -> inspectTranslation ts
  Special element   -> inspectSpecial element

toMap :: (Bounded l, Enum l, Ord l) => (l -> o) -> Map l o
toMap f = Map.fromList $ map (second f . dupe) [minBound .. maxBound]
