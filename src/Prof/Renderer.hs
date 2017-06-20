{-# LANGUAGE RecordWildCards, TupleSections #-}
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
    H.link ! AH.rel (toValue "stylesheet") ! AH.href (toValue "style.css")
  H.body $ do
    H.pre $ do
      H.code $ string (dropWhile isSpace prJob)
    H.ul ! A.id_ (toValue "legend") $ do
      for_ (Map.toList prNames) $ \ (costCentreId, name) ->
        H.li ! A.id_ (toValue costCentreId) ! AH.style (toValue "color: " `mappend` colour costCentreId 1.0) $ string (B.unpack name)
    H.div ! AH.class_ (toValue "graph") $
      S.svg
      ! S.customAttribute (S.stringTag "xmlns") (S.toValue "http://www.w3.org/2000/svg")
      ! S.customAttribute (S.stringTag "xmlns:xlink") (S.toValue "http://www.w3.org/1999/xlink")
      ! A.width (toValue graphWidth)
      ! A.height (toValue graphHeight) $ do
        S.g ! A.transform (S.translate 0 graphHeight `mappend` S.scale 60 (-60/1024/1024)) $
          foldr (>>) (pure ()) $ Map.mapWithKey toPath . Map.unionsWith (<>) . fmap (fmap pure) $ zipWith toMap [0..] (reverse prSamples)
  where toPath :: CostCentreId -> [(Int, Time, Double)] -> S.Svg
        toPath costCentreId points = S.path ! A.d (S.mkPath (snd (foldl' step (pred 0, S.m 0 0) points))) ! A.id_ (S.toValue costCentreId) ! A.stroke (colour costCentreId 1) ! A.fill (colour costCentreId 0.5)
        step (prevI, steps) (i, x, y) = (,) i . (steps >>) $ if prevI < pred i then do
          S.m x (0 :: Double)
          S.l x y
        else
          S.l x y
        toMap :: Int -> (Time, ProfileSample) -> Map.IntMap (Int, Time, Double)
        toMap i (time, samples) = Map.fromList (fmap ((i, time,) . fromIntegral) <$> samples)
        graphWidth = maybe (60 :: Int) ((* 60) . ceiling . fst . fst) (uncons prSamples)
        graphHeight = maybe (60 :: Int) ((* 60) . ceiling . (* (1/1024/1024)) . fromIntegral . maximum . fmap (snd . maximumBy (compare `on` snd) . snd)) (nonEmpty prSamples)

        colour :: CostCentreId -> Double -> AttributeValue
        colour costCentreId alpha =
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
          in stringValue $ "rgba(" <> show (ceiling (r * 255)) <> ", " <> show (ceiling (g * 255)) <> ", " <> show (ceiling (b * 255)) <> ", " <> show alpha <> ")"

        inRange x (l, u) = l <= x && x <= u


printRendering :: FilePath -> FilePath -> IO ()
printRendering inputPath outputPath = do
  profile <- readProfile inputPath
  case renderSvg . renderProfile <$> profile of
    Just svg -> B.writeFile outputPath svg
    _ -> error "Could not read profile."
