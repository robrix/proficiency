{-# LANGUAGE RecordWildCards, TupleSections #-}
module Prof.Renderer where

import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldl')
import qualified Data.IntMap as Map
import Data.Semigroup
import Profiling.Heap.Read (readProfile)
import Profiling.Heap.Types
import Text.Blaze.Svg.Renderer.Pretty
import Text.Blaze
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A


renderProfile :: Profile -> S.Svg
renderProfile Profile{..} = do
  S.preEscapedString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<?xml-stylesheet href=\"style.css\" type=\"text/css\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
  S.svg ! S.customAttribute (S.stringTag "xmlns") (S.toValue "http://www.w3.org/2000/svg") ! S.customAttribute (S.stringTag "xmlns:xlink") (S.toValue "http://www.w3.org/1999/xlink") $ do
    S.title $ S.string (prJob <> " " <> prDate)

    S.text_ ! A.x (S.toValue (10 :: Int)) ! A.y (S.toValue (10 :: Int)) $ S.string (prJob <> " " <> prDate)

    S.g ! A.transform (S.translate 10 30 `mappend` S.scale 60 (10/1024/1024)) $
      foldr (>>) (pure ()) $ Map.mapWithKey toPath . Map.unionsWith (<>) . fmap (fmap (:[])) $ zipWith toMap [0..] (reverse prSamples)
  where toPath :: CostCentreId -> [(Int, Time, Double)] -> S.Svg
        toPath costCentreID points = S.path ! A.d (S.mkPath (snd (foldl' step (pred 0, S.m 0 0) points))) ! A.id_ (S.toValue costCentreID)
        step (prevI, steps) (i, x, y) = (,) i . (steps >>) $ if prevI < pred i then do
          S.m x (0 :: Double)
          S.l x y
        else
          S.l x y
        toMap :: Int -> (Time, ProfileSample) -> Map.IntMap (Int, Time, Double)
        toMap i (time, samples) = Map.fromList (fmap ((i, time,) . fromIntegral) <$> samples)


printRendering :: FilePath -> FilePath -> IO ()
printRendering inputPath outputPath = do
  profile <- readProfile inputPath
  case renderSvg . renderProfile <$> profile of
    Just svg -> writeFile outputPath svg
    _ -> error "Could not read profile."
