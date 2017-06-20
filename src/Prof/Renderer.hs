{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
module Prof.Renderer where

import qualified Data.ByteString.Lazy as B hiding (unpack)
import qualified Data.ByteString.Char8 as B (unpack)
import Data.Char
import Data.Fixed
import Data.Foldable (foldl', for_)
import Data.Function
import qualified Data.IntMap as Map
import Data.List as List
import Data.List.NonEmpty (nonEmpty)
import Data.Ord
import Data.Semigroup
import Profiling.Heap.Read (readProfile)
import Profiling.Heap.Types
import Text.Blaze.Svg.Renderer.Utf8
import Text.Blaze
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as AH
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A


renderProfile :: Profile -> S.Svg
renderProfile Profile{..} = H.docTypeHtml $ do
  H.head $ do
    H.title $ string (prJob <> " " <> prDate)
    H.link ! AH.rel "stylesheet" ! AH.href "style.css"
    H.script ! AH.type_ "text/javascript" ! AH.src "legend.js" $ pure ()
  H.body $ do
    H.pre $ do
      H.code $ string (dropWhile isSpace prJob)
    H.ul ! A.id_ "legend" $ do
      H.input ! AH.type_ "search"
      for_ (Map.toList prNames) $ \ (costCentreId, name) ->
        H.li ! A.id_ (stringValue ("legend-" <> show costCentreId)) ! dataAttribute "id" (toValue costCentreId) ! AH.style ("color: " `mappend` colour costCentreId) $ do
          H.label $ do
            H.input ! AH.type_ "checkbox" ! AH.checked ""
            string (B.unpack name)
    H.div ! AH.class_ "graph" $
      S.svg
      ! S.customAttribute "xmlns" "http://www.w3.org/2000/svg"
      ! S.customAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
      ! A.width (toValue (graphWidth + 40))
      ! A.height (toValue (graphHeight + 20)) $ do
        S.g ! A.transform (S.translate 5 (5 :: Int)) $ do
          S.g ! A.id_ "graph" ! A.transform (S.translate 20 graphHeight `mappend` S.scale 60 (-60)) $ do
            S.g ! A.id_ "overlaid" $ do
              foldr (>>) (pure ()) $ Map.mapWithKey toPath . Map.unionsWith (<>) . fmap (fmap pure) $ zipWith toMap [0..] (reverse prSamples)
            S.g ! A.id_ "grid" $ do
              for_ [0..graphSeconds] $ \ i -> do
                S.line ! A.x1 (toValue i) ! A.x2 (toValue i) ! A.y1 (toValue (0 :: Int)) ! A.y2 (toValue graphMBs)
              for_ [0..graphMBs] $ \ i -> do
                S.line ! A.x1 (toValue (0 :: Int)) ! A.x2 (toValue graphSeconds) ! A.y1 (toValue i) ! A.y2 (toValue i)
          S.g ! A.transform (S.translate 20 (graphHeight + 5)) ! A.class_ "axis x" $ do
            for_ [0..graphSeconds] $ \ i -> do
              S.text_ ! A.x (toValue (i * 60)) ! A.y (toValue (0 :: Int)) ! A.class_ "label x" $ string (show i <> "s")
          S.g ! A.transform (S.translate 15 0) ! A.class_ "axis y" $ do
            for_ [0..graphMBs] $ \ i -> do
              S.text_ ! A.x (toValue (0 :: Int)) ! A.y (toValue (graphHeight - i * 60)) ! A.class_ "label y" $ string (show i <> "M")
    H.script ! AH.type_ "text/javascript" $ string "run();"

  where toPath :: CostCentreId -> [(Int, Time, Double)] -> S.Svg
        toPath costCentreId points = S.path ! A.d (S.mkPath p) ! A.id_ (toValue ("path-" <> show costCentreId)) ! dataAttribute "id" (toValue costCentreId) ! A.stroke (colour costCentreId) ! A.fill (colour costCentreId)
          where p = let (_, x, path) = foldl' step (pred 0, 0, S.m 0 0) points in path >> S.l x 0
        step (prevI, prevX, steps) (i, x, y) = (i,x,) . (steps >>) $ if prevI < pred i then do
          S.l x 0
          S.m x (0 :: Double)
          S.l x y
        else
          S.l x y
        toMap :: Int -> (Time, ProfileSample) -> Map.IntMap (Int, Time, Double)
        toMap i (time, samples) = Map.fromList (fmap ((i, time,) . (* (1/1024/1024)) . fromIntegral) <$> samples)
        graphSeconds = maybe (1 :: Int) (ceiling . fst . fst) (uncons prSamples)
        graphMBs = maybe (1 :: Int) (ceiling . (* (1/1024/1024)) . fromIntegral . maximum . fmap (snd . maximumBy (compare `on` snd) . snd)) (nonEmpty prSamples)
        graphWidth = graphSeconds * 60
        graphHeight = graphMBs * 60

        colour :: CostCentreId -> AttributeValue
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


printRendering :: FilePath -> FilePath -> IO ()
printRendering inputPath outputPath = do
  profile <- readProfile inputPath
  case renderSvg . renderProfile <$> profile of
    Just svg -> B.writeFile outputPath svg
    _ -> error "Could not read profile."
