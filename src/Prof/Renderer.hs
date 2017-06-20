{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
module Prof.Renderer where

import Control.Applicative
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Lazy as B hiding (unpack)
import qualified Data.ByteString.Char8 as B (unpack)
import Data.Char
import Data.Fixed
import Data.Foldable (foldl', for_, toList)
import Data.Function
import qualified Data.IntMap as IntMap
import Data.List as List
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified GHC.Prof as Prof
import Profiling.Heap.Read (readProfile)
import qualified Profiling.Heap.Types as Hp
import System.FilePath.Glob
import Text.Blaze.Svg.Renderer.Utf8
import Text.Blaze
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as AH
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A


renderProfile :: Hp.Profile -> Prof.Profile -> S.Svg
renderProfile Hp.Profile{..} prof = H.docTypeHtml $ do
  H.head $ do
    H.title $ string (prJob <> " " <> prDate)
    H.link ! AH.rel "stylesheet" ! AH.href "style.css"
    H.script ! AH.type_ "text/javascript" ! AH.src "legend.js" $ pure ()
  H.body $ do
    H.pre $ do
      H.code $ string (dropWhile isSpace prJob)
    H.aside ! A.id_ "legend" $ do
      H.input ! AH.type_ "checkbox" ! AH.checked "" ! AH.id "toggle-all"
      H.input ! AH.type_ "search" ! AH.id "filter-legend"
      H.ul $ do
        for_ (toList costCentresByProfId) $ \ cc@CostCentre{..} -> case costCentreHpId of
          Just _ -> H.li
            ! A.id_ (stringValue ("legend-" <> show costCentreProfId))
            ! dataAttribute "id" (toValue costCentreProfId)
            ! dataAttribute "name" (toValue (formattedName cc))
            ! AH.style ("color: " `mappend` colour costCentreProfId) $ do
            H.label $ do
              H.input ! AH.type_ "checkbox" ! AH.checked "" ! dataAttribute "id" (toValue costCentreProfId)
              case costCentreSource of
                Just source | source `notElem` ["<no location info", "<entire-module", "<built-in"] -> H.a ! AH.title (toValue source) $ string $ formattedName cc
                _ -> string $ formattedName cc
          _ -> pure ()
    H.div ! AH.class_ "graph" $
      S.svg
      ! S.customAttribute "xmlns" "http://www.w3.org/2000/svg"
      ! S.customAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
      ! A.width (toValue (graphWidth + 40))
      ! A.height (toValue (graphHeight + 20)) $ do
        S.g ! A.transform (S.translate 25 (5 :: Int)) $ do
          S.g ! A.id_ "graph" ! A.transform (S.translate 0 graphHeight `mappend` S.scale 60 (-60)) $ do
            S.g ! A.id_ "overlaid" $ do
              foldr (>>) (pure ()) $ IntMap.mapWithKey toPath costCentreHpMap
            S.g ! A.id_ "grid" $ do
              for_ [0..graphSeconds] $ \ i -> do
                S.line ! A.x1 (toValue i) ! A.x2 (toValue i) ! A.y1 (toValue (0 :: Int)) ! A.y2 (toValue graphMBs)
              for_ [0..graphMBs] $ \ i -> do
                S.line ! A.x1 (toValue (0 :: Int)) ! A.x2 (toValue graphSeconds) ! A.y1 (toValue i) ! A.y2 (toValue i)
          S.g ! A.transform (S.translate 0 (graphHeight + 5)) ! A.class_ "axis x" $ do
            for_ [0..graphSeconds] $ \ i -> do
              S.text_ ! A.x (toValue (i * 60)) ! A.y (toValue (0 :: Int)) ! A.class_ "label x" $ string (show i <> "s")
          S.g ! A.class_ "axis y" $ do
            for_ [0..graphMBs] $ \ i -> do
              S.text_ ! A.x (toValue ((-5) :: Int)) ! A.y (toValue (graphHeight - i * 60)) ! A.class_ "label y" $ string (show i <> "M")
    H.script ! AH.type_ "text/javascript" $ string "run();"

  where toPath :: Hp.CostCentreId -> (CostCentre, [(Int, Hp.Time, Double)]) -> S.Svg
        toPath hpId (CostCentre{..}, points) = S.path ! A.d (S.mkPath p) ! A.id_ (toValue ("path-" <> show costCentreProfId)) ! dataAttribute "id" (toValue costCentreProfId) ! A.stroke (colour costCentreProfId) ! A.fill (colour costCentreProfId)
          where p = let (_, x, path) = foldl' step (pred 0, 0, S.m 0 0) points in path >> S.l x 0
        step (prevI, prevX, steps) (i, x, y) = (i,x,) . (steps >>) $ if prevI < pred i then do
          S.l x 0
          S.m x (0 :: Double)
          S.l x y
        else
          S.l x y
        hpSamplesMap = IntMap.unionsWith (<>) (fmap pure <$> zipWith toMap [0..] (reverse prSamples))
        costCentreHpMap = IntMap.mapWithKey (\ hpId samples -> (costCentreForHpId hpId, samples)) hpSamplesMap
        toMap :: Int -> (Hp.Time, Hp.ProfileSample) -> IntMap.IntMap (Int, Hp.Time, Double)
        toMap i (time, samples) = IntMap.fromList (fmap ((i, time,) . (* (1/1024/1024)) . fromIntegral) <$> samples)
        graphSeconds = maybe (1 :: Int) (ceiling . fst . fst) (uncons prSamples)
        graphMBs = maybe (1 :: Int) (ceiling . (* (1/1024/1024)) . fromIntegral . maximum . fmap (snd . maximumBy (compare `on` snd) . snd)) (nonEmpty prSamples)
        graphWidth = graphSeconds * 60
        graphHeight = graphMBs * 60

        colour :: Hp.CostCentreId -> AttributeValue
        colour costCentreId =
          let hue = (35 * fromIntegral costCentreId) `mod'` 360
              saturation = 1
              value = 0.5
              hue' = hue / 60
              chroma = value * saturation
              x = chroma * (1 - abs ((hue' `mod'` 2) - 1))
              (r, g, b) = case hue' of
                _ | hue' `inRange` (0, 1) -> (chroma, x, 0)
                _ | hue' `inRange` (1, 2) -> (x, chroma, 0)
                _ | hue' `inRange` (2, 3) -> (0, chroma, x)
                _ | hue' `inRange` (3, 4) -> (0, x, chroma)
                _ | hue' `inRange` (4, 5) -> (x, 0, chroma)
                _ | hue' `inRange` (5, 6) -> (chroma, 0, x)
          in stringValue $ "rgb(" <> show (ceiling (r * 255)) <> ", " <> show (ceiling (g * 255)) <> ", " <> show (ceiling (b * 255)) <> ")"

        inRange x (l, u) = l <= x && x <= u

        hpIdsByProfId = IntMap.fromList $ uncurry pair <$> IntMap.toList prNames
          where pair hpId name
                  | Just ((profId, _), _) <- uncons (readParen True reads (B.unpack name)) = (profId, hpId)
                  | Just profId <- Map.lookup (B.unpack name) profIdsByName                = (profId, hpId)
                  | otherwise                                                              = (-1,     hpId)

        profCentres = foldMap (foldMap (\ cc -> [((Prof.costCentreNo cc, T.unpack (Prof.costCentreName cc)), toCC cc)])) (Prof.costCentresOrderBy Prof.costCentreNo prof)
        (costCentresByProfId, costCentresByName, profIdsByName) =
          let profCCsByProfId = IntMap.fromList (first fst <$> profCentres)
              profCCsByProfName = Map.fromList (first snd <$> profCentres) in
          ( profCCsByProfId `IntMap.union` IntMap.fromList (fmap (\ (hpId, hpName) -> let cc = costCentreForHpIdAndNameIn profCCsByProfId profCCsByProfName hpId (B.unpack hpName) in (costCentreProfId cc, cc)) (IntMap.toList prNames))
          , profCCsByProfName
          , Map.fromList (bimap snd costCentreProfId <$> profCentres))
        toCC Prof.CostCentre{..} = CostCentre costCentreNo (IntMap.lookup costCentreNo hpIdsByProfId) (T.unpack costCentreName) (Just (T.unpack costCentreModule)) (fmap T.unpack costCentreSrc)

        costCentreForHpId hpId = let hpName = B.unpack $ prNames IntMap.! hpId in costCentreForHpIdAndNameIn costCentresByProfId costCentresByName hpId hpName
        costCentreForHpIdAndNameIn byId byName hpId name = case uncons (readParen True reads name) of
          Just ((profId, rest), _) -> fromMaybe (CostCentre profId (Just hpId) rest Nothing Nothing) (IntMap.lookup profId byId)
          _ -> fromMaybe (CostCentre (-1) (IntMap.lookup (-1) hpIdsByProfId) name Nothing Nothing) (Map.lookup name costCentresByName)

        formattedName CostCentre{..} = maybe costCentreName (<> "." <> costCentreName) costCentreModuleName

data CostCentre = CostCentre
  { costCentreProfId :: Int
  , costCentreHpId :: Maybe Int
  , costCentreName :: String
  , costCentreModuleName :: Maybe String
  , costCentreSource :: Maybe String
  }

printRendering :: FilePath -> FilePath -> IO ()
printRendering profilePath outputPath = do
  ([[hpPath], [profPath]], _) <- globDir [compile "*.hp", compile "*.prof"] profilePath
  hp <- readProfile hpPath
  profFile <- T.readFile profPath
  let prof = either error Just (Prof.decode profFile)
  case (renderSvg .) . renderProfile <$> hp <*> prof of
    Just svg -> B.writeFile outputPath svg
    _ -> error "Could not read profile."
