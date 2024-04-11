{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Output.Part
    ( OutputPart(..)
    , getOutputParts
    , getOutputPartsWithRating
    , toOutputMonad
    ) where


import Control.Monad.Output
import Control.Monad.Output.Generic (runLangMReportMultiLang)
import Control.Monad.Trans.State    (put)
import Data.Foldable                (for_)
import Data.Map                     (Map)
import Data.Maybe                   (fromMaybe)
import Data.Tuple.Extra             (dupe, both, first, second)
import GHC.Generics                 (Generic)

import qualified Data.Map as Map



data OutputPart
    = Assertion Bool [OutputPart]
    | Image FilePath
    | Images (Map String FilePath)
    | Paragraph [OutputPart]
    | Refuse [OutputPart]
    | Enumerated [([OutputPart],[OutputPart])]
    | Itemized [[OutputPart]]
    | Indented [OutputPart]
    | Latex String
    | Code (Map Language String)
    | Translated (Map Language String)
    deriving (Eq, Generic, Read, Show)


instance Monad m => GenericOutputMonad Language (ReportT OutputPart m) where
  assertion b = alignOutput (Assertion b)

  image = format . Image

  images descF fileF = format . Images . Map.mapKeys descF . Map.map fileF

  paragraph = alignOutput Paragraph

  refuse = alignOutput Refuse

  enumerateM f tups = combineReports (Enumerated . concatMap expose . concat) combine
    where
      combine = map (uncurry (combineTwoReports $ curry $ Enumerated . (:[])) . first f) tups

      expose (Enumerated list) = list
      expose _                 = error "This is impossible"

  itemizeM = combineReports Itemized

  indent = alignOutput Indented

  latex = format . Latex

  translatedCode =  format . Code . toMap

  translated = format . Translated . toMap


toOutputMonad :: OutputMonad m => [OutputPart] -> LangM m
toOutputMonad parts = for_ parts toInterface
  where
    toInterface res = case res of
      Assertion b xs   -> assertion b $ toOutputMonad xs
      Image path       -> image path
      Images m         -> images id id m
      Paragraph xs     -> paragraph $ toOutputMonad xs
      Refuse xs        -> refuse $ toOutputMonad xs
      Enumerated list  -> enumerateM id $ map (both toOutputMonad) list
      Itemized xs      -> itemizeM $ map toOutputMonad xs
      Indented xs      -> indent $ toOutputMonad xs
      Latex s          -> latex s
      Code m           -> translateCode $ put m
      Translated m     -> translate $ put m

    translateCode xs = translatedCode $ \l ->
                         fromMaybe "" $ Map.lookup l $ translations xs



toMap :: (Bounded l, Enum l, Ord l) => (l -> o) -> Map l o
toMap f = Map.fromList $ map (second f . dupe) [minBound .. maxBound]



getOutputParts :: Monad m => LangM (ReportT OutputPart m) -> m [OutputPart]
getOutputParts = (snd <$>) . getOutputAnyResult


getOutputPartsWithRating
  :: Monad m
  => Rated (ReportT OutputPart m)
  -> m (Maybe Rational,[OutputPart])
getOutputPartsWithRating = getOutputAnyResult


getOutputAnyResult
  :: Monad m
  => LangM' (ReportT OutputPart m) a
  -> m (Maybe a,[OutputPart])
getOutputAnyResult lm = second unbox <$>
    runLangMReportMultiLang (Paragraph []) gather ($ English) lm
  where
    gather (Paragraph xs) x = Paragraph (xs ++ [x])
    gather  _ _ = error "this is impossible"

    unbox (Paragraph xs) = xs
    unbox  _ = error "this is impossible"
