{-# LANGUAGE RecordWildCards, TupleSections #-}
module Prof.Renderer where

import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldl')
import qualified Data.IntMap as Map
import Data.Semigroup
import Profiling.Heap.Read (readProfile)
import Profiling.Heap.Types
import Text.Blaze.Svg.Renderer.Pretty
import Text.Blaze.Svg11
import qualified Text.Blaze.Svg11.Attributes as A


renderProfile :: Profile -> Svg
renderProfile Profile{..} = do
  preEscapedString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<?xml-stylesheet href=\"style.css\" type=\"text/css\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
  svg ! customAttribute (stringTag "xmlns") (stringValue "http://www.w3.org/2000/svg") ! customAttribute (stringTag "xmlns:xlink") (stringValue "http://www.w3.org/1999/xlink") $ do
    title $ string (prJob <> " " <> prDate)

    text_ ! A.x (toValue (10 :: Int)) ! A.y (toValue (10 :: Int)) $ string (prJob <> " " <> prDate)

    g ! A.transform (translate 10 30 `mappend` scale 60 (10/1024/1024)) $
      foldr (>>) (pure ()) $ Map.mapWithKey toPath . Map.unionsWith (<>) . fmap (fmap (:[])) $ zipWith toMap [0..] (reverse prSamples)
  where toPath :: CostCentreId -> [(Int, Time, Double)] -> Svg
        toPath costCentreID points = path ! A.d (mkPath (snd (foldl' step (pred 0, m 0 0) points))) ! A.id_ (toValue costCentreID)
        step (prevI, steps) (i, x, y) = (,) i . (steps >>) $ if prevI < pred i then do
          m x (0 :: Double)
          l x y
        else
          l x y
        toMap :: Int -> (Time, ProfileSample) -> Map.IntMap (Int, Time, Double)
        toMap i (time, samples) = Map.fromList (fmap ((i, time,) . fromIntegral) <$> samples)


printRendering :: FilePath -> FilePath -> IO ()
printRendering inputPath outputPath = do
  profile <- readProfile inputPath
  case renderSvg . renderProfile <$> profile of
    Just svg -> writeFile outputPath svg
    _ -> error "Could not read profile."
