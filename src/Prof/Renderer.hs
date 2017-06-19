{-# LANGUAGE RecordWildCards, TupleSections #-}
module Prof.Renderer where

import qualified Data.ByteString.Lazy as B hiding (unpack)
import qualified Data.ByteString.Char8 as B (unpack)
import Data.Char
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
        H.li ! A.id_ (toValue costCentreId) $ string (B.unpack name)
    H.div ! AH.class_ (toValue "graph") $
      S.svg
      ! S.customAttribute (S.stringTag "xmlns") (S.toValue "http://www.w3.org/2000/svg")
      ! S.customAttribute (S.stringTag "xmlns:xlink") (S.toValue "http://www.w3.org/1999/xlink")
      ! A.width (toValue (maybe (60 :: Int) ((* 60) . ceiling . fst . fst) (uncons prSamples)))
      ! A.height (toValue (maybe (60 :: Int) ((* 60) . ceiling . (* (1/1024/1024)) . fromIntegral . maximum . fmap (snd . maximumBy (compare `on` snd) . snd)) (nonEmpty prSamples))) $ do
        S.title $ S.string (prJob <> " " <> prDate)

        S.g ! A.transform (S.scale 60 (60/1024/1024)) $
          foldr (>>) (pure ()) $ Map.mapWithKey toPath . Map.unionsWith (<>) . fmap (fmap (:[])) $ zipWith toMap [0..] (reverse prSamples)
  where toPath :: CostCentreId -> [(Int, Time, Double)] -> S.Svg
        toPath costCentreId points = S.path ! A.d (S.mkPath (snd (foldl' step (pred 0, S.m 0 0) points))) ! A.id_ (S.toValue costCentreId)
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
    Just svg -> B.writeFile outputPath svg
    _ -> error "Could not read profile."
