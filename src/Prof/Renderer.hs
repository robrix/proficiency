{-# LANGUAGE RecordWildCards #-}
module Prof.Renderer where

import Data.Semigroup
import Profiling.Heap.Read (readProfile)
import Profiling.Heap.Types (Profile(..))
import Text.Blaze.Svg.Renderer.Pretty
import Text.Blaze.Svg11
import qualified Text.Blaze.Svg11.Attributes as A


renderProfile :: Profile -> Svg
renderProfile Profile{..} = do
  preEscapedString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<?xml-stylesheet href=\"style.css\" type=\"text/css\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
  svg ! customAttribute (stringTag "xmlns") (stringValue "http://www.w3.org/2000/svg") ! customAttribute (stringTag "xmlns:xlink") (stringValue "http://www.w3.org/1999/xlink") $ do
    title $ string (prJob <> " " <> prDate)

    text_ ! A.x (toValue (10 :: Int)) ! A.y (toValue (10 :: Int)) $ string (prJob <> " " <> prDate)


printRendering :: FilePath -> FilePath -> IO ()
printRendering inputPath outputPath = do
  profile <- readProfile inputPath
  case renderSvg . renderProfile <$> profile of
    Just svg -> writeFile outputPath svg
    _ -> error "Could not read profile."
