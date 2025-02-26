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
    , withRefusal
    ) where

import Control.OutputCapable.Blocks.Generic (
  GenericLangM,
  GenericOutputCapable (..),
  GenericReportT,
  alignOutput,
  combineReports,
  combineTwoReports,
  format,
  runLangMReportMultiLang,
  translate,
  translateCode,
  )
import Control.Monad.Trans.State    (put)
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
    = Assertion Bool [GenericOutput language element]
    -- ^ abortion is expected at 'False'
    | Image FilePath
    -- ^ a single image
    | Images (Map String FilePath)
    -- ^ multiple images with a text tag that is to be printed (e.g. a number)
    | Paragraph [GenericOutput language element]
    -- ^ to group (and separate) output
    | Refuse [GenericOutput language element]
    -- ^ abort with output
    | Enumerated [([GenericOutput language element], [GenericOutput language element])]
    -- ^ an enumeration with the enumerator first and content second
    | Itemized [[GenericOutput language element]]
    -- ^ like enumeration (but without enumerator)
    | Indented [GenericOutput language element]
    -- ^ for indenting output
    | Latex String
    -- ^ latex code (for formulas and text blocks only)
    | Code (Map language String)
    -- ^ to output as text with fixed width, providing translations
    | Translated (Map language String)
    -- ^ normal text with translations
    | Special element
    -- ^ allows abbreviating several complex parts
    --   which have special rendering functions
    --   which are not easily representable as ADT
    deriving (Eq, Functor, Foldable, Generic, Read, Show)

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
  assertion b = alignOutput (Assertion b)

  image = format . Image

  images descF fileF = format . Images . Map.mapKeys descF . Map.map fileF

  paragraph = alignOutput Paragraph

  refuse = alignOutput Refuse

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

  translatedCode =  format . Code . toMap

  translated = format . Translated . toMap



{- |
Convert a list of 'GenericOutput' into any member of 'GenericOutputCapable'
-}
toOutputCapable
  :: GenericOutputCapable language m
  => (element -> GenericLangM language m ())
  -> [GenericOutput language element]
  -> GenericLangM language m ()
toOutputCapable toOutputPart parts = for_ parts toInterface
  where
    toInterface res = case res of
      Assertion b xs   -> assertion b $ toOutputCapable' xs
      Image path       -> image path
      Images m         -> images id id m
      Paragraph xs     -> paragraph $ toOutputCapable' xs
      Refuse xs        -> refuse $ toOutputCapable' xs
      Enumerated list  -> enumerateM id $ map (both toOutputCapable') list
      Itemized xs      -> itemizeM $ map toOutputCapable' xs
      Indented xs      -> indent $ toOutputCapable' xs
      Latex s          -> latex s
      Code m           -> translateCode $ put m
      Translated m     -> translate $ put m
      Special element  -> toOutputPart element
    toOutputCapable' = toOutputCapable toOutputPart



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
  Assertion _ xs    -> foldr (f . descend) (evaluate x) xs
  Image {}          -> evaluate x
  Images {}         -> evaluate x
  Paragraph xs      -> foldr (f . descend) (evaluate x) xs
  Refuse xs         -> foldr (f . descend) (evaluate x) xs
  Enumerated xs   ->
    foldr
    (\next done -> uncurry f $ both (foldr (f . descend) done) next)
    (evaluate x)
    xs
  Itemized xs       -> foldr (flip (foldr (f . descend))) (evaluate x) xs
  Indented xs       -> foldr (f . descend) (evaluate x) xs
  Latex {}          -> evaluate x
  Code {}           -> evaluate x
  Translated {}     -> evaluate x
  Special {}        -> evaluate x
  where
    descend = foldMapOutputBy f evaluate

{-|
Checks whether any refusal exists within the given 'GenericOutput'.

@since: 0.3.0.1
-}
withRefusal :: (element -> Bool) -> GenericOutput language element -> Bool
withRefusal checkSpecial = foldMapOutputBy (||) $ \case
  Assertion False _ -> True
  Assertion True _  -> False
  Image {}          -> False
  Images {}         -> False
  Paragraph {}      -> False
  Refuse {}         -> True
  Enumerated {}     -> False
  Itemized {}       -> False
  Indented {}       -> False
  Latex {}          -> False
  Code {}           -> False
  Translated {}     -> False
  Special element   -> checkSpecial element

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
  Assertion {}      -> z
  Image {}          -> z
  Images {}         -> z
  Paragraph {}      -> z
  Refuse {}         -> z
  Enumerated {}     -> z
  Itemized {}       -> z
  Indented {}       -> z
  Latex {}          -> z
  Code ts           -> inspectTranslation ts
  Translated ts     -> inspectTranslation ts
  Special element   -> inspectSpecial element

toMap :: (Bounded l, Enum l, Ord l) => (l -> o) -> Map l o
toMap f = Map.fromList $ map (second f . dupe) [minBound .. maxBound]
