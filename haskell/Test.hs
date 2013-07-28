-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
import Graphics.Transform.MagickMirror
import Control.Monad
import Control.Applicative
import qualified Data.Map as M

-- on my embarassing centrino laptop, the whole test suite runs clean in 2m13.955s
main = do
  putStrLn "convert"
  testConvert
  putStrLn "shadow"
  testShadow
  putStrLn "focus"
  testFocus
  putStrLn "UV"
  testUV
  putStrLn "adjustments"
  testAdjusts
  putStrLn "blank canvas"
  testBlankCanvas
  return ()

-- shows atlantis, fading into saturn
-- there's an aspect ratio difference between the images, which makes it look odd
testConvert = runMagick "images/AtlantisReflection_ingalls.jpg" "cached_images" $ do
  format "png"
  resize (showGeometry 800 600 0 0)
  autoCheckpoint
  gradient <- runLayer "images/mask.png" $ do
    resize (showGeometry 800 600 0 0)
  saturn <- runLayer "images/saturnequinox_cassini_big.jpg" $ do
    format "png"
    resize (showGeometry 800 600 0 0)
  blend saturn gradient
  checkpoint "testconvert.png"

-- crop atlantis and drop-shadow it
testShadow = runMagick "images/AtlantisReflection_ingalls.jpg" "cached_images" $ do
  flipImage
  format "png"
  resize (showGeometry 800 600 0 0)
  crop (showGeometry 100 100 300 200)
  shadow "black" "100x10+5+5"
  checkpoint "testshadow.png"

-- print exif data for atlantis
testIdentify = do
  m <- M.toList <$> getExif "images/AtlantisReflection_ingalls.jpg"
  forM_ m $ \(k, v) -> do
    putStrLn (k ++ " - " ++ show v)

-- focus on water tower
testFocus = runMagick "images/AtlantisReflection_ingalls.jpg" "cached_images" $ do
  resize (showGeometry 800 600 0 0)
  m <- runLayer "images/AtlantisReflection_towermask.png" $ do
    resize (showGeometry 800 600 0 0)
  focus m "7"
  checkpoint "testfocus.png"

-- blur water tower horizontally and everything else vertically
testUV = runMagick "images/AtlantisReflection_ingalls.jpg" "cached_images" $ do
  resize (showGeometry 800 600 0 0)
  r <- runLayer "images/AtlantisReflection_towermask.png" $ do
    resize (showGeometry 800 600 0 0)
  g <- runLayer r $ do
    negateImage
  uv <- makeUV r g
  blur uv "5"
  checkpoint "testuv.png"

testAdjusts = do
  mapM_ (runMagick "images/AtlantisReflection_ingalls.jpg" "cached_images") $ [do
    resize (showGeometry 800 600 0 0)
    sigmoid 10 50
    checkpoint "sigmoid-10-50.jpg"
   , do
    resize (showGeometry 800 600 0 0)
    sigmoid 10 80
    checkpoint "sigmoid-10-80.jpg"
   , do
    resize (showGeometry 800 600 0 0)
    level (-10) 95 1.5
    checkpoint "level--10-95-15.jpg"
   , do
    resize (showGeometry 800 600 0 0)
    gamma 0.5
    checkpoint "gamma-05.jpg"
   ]

testBlankCanvas = do
  runCanvas 300 150 "cached_images" $ do
    return ()

