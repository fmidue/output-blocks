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
import Control.Monad.Trans.State    (State, put)
import Data.Foldable
import Data.Map                     (Map)
import Data.Maybe                   (fromMaybe)
import Data.Tuple.Extra             (dupe, first, second)
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
  -- | for assertions, i.e. expected behaviour is explanation
  -- (and abortion on 'False')
  assertion b = alignOutput (Assertion b)
  -- | for printing a single image from file
  image = format . Image
  -- | for printing multiple images using the given map
  images descF fileF = format . Images . Map.mapKeys descF . Map.map fileF
  -- | for a complete paragraph
  paragraph = alignOutput Paragraph
  -- | should abort at once
  refuse = alignOutput Refuse
  -- | for an enumerated sequence of elements
  enumerateM f tups = combineReports (Enumerated . concatMap expose . concat) combine
    where
      combine = map (uncurry (combineTwoReports $ curry $ Enumerated . (:[])) . first f) tups

      expose (Enumerated list) = list
      expose _                 = error "This is impossible"
  -- | for an unenumerated sequence of elements
  itemizeM = combineReports Itemized
  -- | for indentation
  indent = alignOutput Indented
  -- | for LaTeX-Math code (i.e. without surrounding @$@)
  latex = format . Latex
  -- | for fixed width fonts (i.e. typewriter style)
  -- | same as 'code', but with different translations
  translatedCode =  format . Code . toMap
  -- | for displaying text with translations
  translated = format . Translated . toMap



toInterface :: OutputMonad m => OutputPart -> LangM m
toInterface res = case res of
  Assertion b xs   -> assertion b $ toOutputMonad xs
  Image path       -> image path
  Images m         -> images id id m
  Paragraph xs     -> paragraph $ toOutputMonad xs
  Refuse xs        -> refuse $ toOutputMonad xs
  Enumerated list  -> enumerateM id $ zip (map (toOutputMonad . fst) list)
                                          (map (toOutputMonad . snd) list)
  Itemized xs      -> itemizeM $ map toOutputMonad xs
  Indented xs      -> indent $ toOutputMonad xs
  Latex s          -> latex s
  Code m           -> translateCode $ put m
  Translated m     -> translate $ put m



translateCode :: OutputMonad m => State (Map Language String) a -> LangM m
translateCode xs = translatedCode $ \l ->
  fromMaybe "" $ Map.lookup l $ translations xs

toOutputMonad :: OutputMonad m => [OutputPart] -> LangM m
toOutputMonad res = for_ res toInterface


toMap :: (Bounded l, Enum l, Ord l) => (l -> o) -> Map l o
toMap f = Map.fromList $ map (second f . dupe) [minBound .. maxBound]


getOutputParts :: Monad m => LangM (ReportT OutputPart m) -> m [OutputPart]
getOutputParts lm = unbox . snd <$> runLangMReportMultiLang (Paragraph []) gather ($ English) lm


unbox :: OutputPart -> [OutputPart]
unbox (Paragraph xs) = xs
unbox  _ = error "this is impossible"

gather :: OutputPart -> OutputPart -> OutputPart
gather (Paragraph xs) x = Paragraph (xs ++ [x])
gather  _ _ = error "this is impossible"


getOutputPartsWithRating :: Monad m => Rated (ReportT OutputPart m) -> m (Maybe Rational,[OutputPart])
getOutputPartsWithRating lm = second unbox <$> runLangMReportMultiLang (Paragraph []) gather ($ English) lm
