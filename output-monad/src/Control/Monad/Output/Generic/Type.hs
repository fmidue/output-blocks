{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Output.Generic.Type
    ( GenericOutput (..)
    , getOutputSequence
    , getOutputSequenceWithRating
    , toOutputMonad
    ) where

import Control.Monad.Output (
  LangM',
  LangM,
  Language (English),
  Rated,
  ReportT,
  )
import Control.Monad.Output.Generic (
  GenericLangM,
  GenericOutputMonad (..),
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
Data type miming the OutputMonad class interface.
Can be used to Prototype without defining additional instances.
The result can be converted into any member of the class.
-}
data GenericOutput language element
    = Assertion Bool [GenericOutput language element]
    | Image FilePath
    | Images (Map String FilePath)
    | Paragraph [GenericOutput language element]
    | Refuse [GenericOutput language element]
    | Enumerated [([GenericOutput language element], [GenericOutput language element])]
    | Itemized [[GenericOutput language element]]
    | Indented [GenericOutput language element]
    | Latex String
    | Code (Map language String)
    | Translated (Map language String)
    | Special element
    -- ^ allows shortcutting several complex parts
    --   which have special rendering functions
    --   which are not easily representable as ADT
    deriving (Eq, Generic, Read, Show)

{-|
OutputMonad instances for 'GenericOutput',
allowing for free conversion between ADT and interface.
-}
instance
  (Bounded language, Enum language, Monad m, Ord language)
  => GenericOutputMonad language (
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
Convert a list of 'GenericOutput' into any member of 'GenericOutputMonad'
-}
toOutputMonad
  :: GenericOutputMonad language m
  => (element -> GenericLangM language m ())
  -> [GenericOutput language element]
  -> GenericLangM language m ()
toOutputMonad toOutputPart parts = for_ parts toInterface
  where
    toInterface res = case res of
      Assertion b xs   -> assertion b $ toOutputMonad' xs
      Image path       -> image path
      Images m         -> images id id m
      Paragraph xs     -> paragraph $ toOutputMonad' xs
      Refuse xs        -> refuse $ toOutputMonad' xs
      Enumerated list  -> enumerateM id $ map (both toOutputMonad') list
      Itemized xs      -> itemizeM $ map toOutputMonad' xs
      Indented xs      -> indent $ toOutputMonad' xs
      Latex s          -> latex s
      Code m           -> translateCode $ put m
      Translated m     -> translate $ put m
      Special element  -> toOutputPart element
    toOutputMonad' = toOutputMonad toOutputPart



{-|
Converts non graded 'GenericOutputMonad' value using 'GenericOutput'
into a list of 'GenericOutput'
-}
getOutputSequence
  :: Monad m
  => LangM (ReportT (GenericOutput language element) m)
  -> m [GenericOutput language element]
getOutputSequence = (snd <$>) . getOutputSequenceAndResult



{-|
More concretely typed alias of 'getOutputSequenceAndResult'

Converts graded 'GenericOutputMonad' value using 'GenericOutput'
into a rating and a list of 'GenericOutput'
-}
getOutputSequenceWithRating
  :: Monad m
  => Rated (ReportT (GenericOutput language element) m)
  -> m (Maybe Rational, [GenericOutput language element])
getOutputSequenceWithRating = getOutputSequenceAndResult



{-|
Converts 'GenericOutputMonad' value using 'GenericOutput'
into a result and a list of 'GenericOutput'
-}
getOutputSequenceAndResult
  :: Monad m
  => LangM' (ReportT (GenericOutput language element) m) a
  -> m (Maybe a, [GenericOutput language element])
getOutputSequenceAndResult lm = second unbox <$>
    runLangMReportMultiLang (Paragraph []) gather ($ English) lm
  where
    gather (Paragraph xs) x = Paragraph (xs ++ [x])
    gather  _ _ = error "this is impossible"

    unbox (Paragraph xs) = xs
    unbox  _ = error "this is impossible"



toMap :: (Bounded l, Enum l, Ord l) => (l -> o) -> Map l o
toMap f = Map.fromList $ map (second f . dupe) [minBound .. maxBound]
