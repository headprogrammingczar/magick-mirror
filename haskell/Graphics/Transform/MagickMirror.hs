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
module Graphics.Transform.MagickMirror where

import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char
import Data.List
import System.Locale
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.ByteString as BS hiding (dropWhile, break, drop, length, isPrefixOf)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString.Char8 as BSC hiding (lines, dropWhile, break, drop, length, isPrefixOf)
import System.FilePath
import System.Directory
import System.Process.ByteString
import System.Exit
import Data.Digest.Pure.SHA

type Image = BS.ByteString

type Exif = Map String ExifType

data ExifType = ES String
              | EI Int
              | ED Double
              | ET POSIXTime
  deriving (Show, Eq)

-- The path of an Image value, separated into filename and dir for easy manipulation
data ImageState = ImageState {filename :: String, workingdir :: String} deriving (Show)

-- A Magick transformation is two separate computations - the inner computation is an image transformation, which will be entirely imagemagick work
-- The outer computation manipulates the start and end file locations for the inner computation, as well as manipulating the computation itself
type Magick a = State Transform a
type Transform = (ImageState, Image -> IO Image)

-- generic version of Magick
type CompileT s r c m a = StateT (s, r -> c r) m a

-- instead of taking a base file like runMagick, take a blank canvas of the given dimensions and use it as a base
runCanvas :: Int -> Int -> FilePath -> Magick () -> IO (Image, ImageState)
runCanvas x y workingdir transform = do
  let filename = workingdir </> "canvas-"++(show x)++"x"++(show y)++".png"
  rawConvert (BSC.pack "") ["-size", (show x) ++ "x" ++ (show y), "canvas:", "-alpha", "transparent", workingdir </> "canvas-"++(show x)++"x"++(show y)++".png"]
  runMagick filename workingdir transform

-- given a reference file and working dir, read the image in, then prepare a working directory
-- build a transformation, then execute the transformation, returning the results of both the prep and the transform
runMagick :: FilePath -> FilePath -> Magick () -> IO (Image, ImageState)
runMagick file workingdir transform = do
  (i, initialState) <- prepFile file workingdir
  let (a, (finalState, f)) = runState (transform >> autoCheckpoint) (initialState, return)
  finalImage <- f i
  return (finalImage, finalState)

runRawMagick :: FilePath -> FilePath -> Magick () -> Transform
runRawMagick file workingdir transform =
  let initialState = prepFilename file workingdir
      (a, resultTrans) = runState (transform >> autoCheckpoint) (initialState, return)
   in resultTrans

readImage :: FilePath -> IO Image
readImage path = BS.readFile path

checkFile :: ImageState -> IO Bool
checkFile s = do
  let path = workingdir s </> filename s
  doesFileExist path

writeToFile :: ImageState -> Image -> IO ()
writeToFile s i = do
  let path = workingdir s </> filename s
  BS.writeFile path i

prepFilename :: FilePath -> FilePath -> ImageState
prepFilename file workingdir = let (_, filename) = splitFileName file in ImageState {filename = filename, workingdir = workingdir}

prepFileContents :: FilePath -> FilePath -> IO Image
prepFileContents file workingdir = do
  direxists <- doesDirectoryExist workingdir
  when (not direxists) $ do
    createDirectory workingdir
  readImage file

prepFile :: FilePath -> FilePath -> IO (Image, ImageState)
prepFile file workingdir = do
  i <- prepFileContents file workingdir
  let initialState = (prepFilename file workingdir)
  return (i, initialState)

autoCheckpoint :: Magick ()
autoCheckpoint = do
  (s, f) <- get
  let path = workingdir s </> filename s
  let f' i = do exists <- checkFile s
                case exists of
                  True -> readImage path
                  False -> do
                    i' <- f i
                    writeToFile s i'
                    return i'
  put (s, f')

checkpoint :: String -> Magick ()
checkpoint path = do
  (s, f) <- get
  let s' = s {filename = path}
  put (s', f)
  autoCheckpoint

prepend prefix s = s {filename = prefix ++ filename s}

mergeStates s s' = s {filename = prefix ++ filename s}
  where
    prefix = hash ++ "-"
    hash = showDigest . sha1 . BSL.pack $ (workingdir s' </> filename s')

-- just execute "convert"
rawConvert :: ByteString -> [String] -> IO ByteString
rawConvert input args = do
  -- match on ExitSuccess so we error out if convert errors
  (exit, out, err) <- readProcessWithExitCode "convert" args input
  case exit of
    ExitSuccess -> return ()
    _ -> BSC.putStrLn err >> exitFailure
  return out

-- this produces incorrect output if identify would return unicode output
-- interestingly, identify itself appears to choke on exif data that includes unicode data
-- keep an eye on this, but it looks like it will not need fixing until identify changes behavior
rawIdentify :: ByteString -> [String] -> IO String
rawIdentify input args = do
  -- match on ExitSuccess so we error out if identify errors
  (exit, out, err) <- readProcessWithExitCode "identify" args input
  case exit of
    ExitSuccess -> return ()
    _ -> BSC.putStrLn err >> exitFailure
  return (BSC.unpack out)

-- pass the necessary parameters to "convert" such that it behaves as a transformation from Image to Image
rawTransform :: [String] -> Image -> IO Image
rawTransform args input = rawConvert input (["-"] ++ args ++ ["-"])

liftTransform :: String -> (Image -> IO Image) -> Magick ()
liftTransform prefix f = do
  (s, f') <- get
  let s' = prepend (prefix ++ "-") s
  let f'' i = do i' <- f' i
                 f i'
  put (s', f'')

transform :: String -> [String] -> Magick ()
transform prefix args = do
  (s, f) <- get
  let s' = prepend (prefix ++ "-") s
  let f' i = do i' <- f i
                rawTransform args i'
  put (s', f')

imagePath :: ImageState -> FilePath
imagePath s = (workingdir s </> filename s)

showGeometry :: Int -> Int -> Int -> Int -> String
showGeometry w h x y = (show w) ++ "x" ++ (show h) ++ (plus x) ++ (plus y)
  where
    plus n | n >= 0 = '+' : show n
           | otherwise = show n

-- run a layered computation within the current one, returning the path of the file it will produce
runLayer :: FilePath -> Magick () -> Magick FilePath
runLayer image trans = do
  (s, f) <- get
  let (sl, fl) = runRawMagick image (workingdir s) trans
  let s' = mergeStates s sl
  let f' i = do i' <- f i
                li <- prepFileContents image (workingdir sl)
                li' <- fl li -- infuriatingly, we can't use li' for anything - we are mainly interested in the "writing an image file" side-effect
                return i'
  put (s', f')
  return (workingdir sl </> filename sl)

runClone :: Magick () -> Magick FilePath
runClone trans = do
  autoCheckpoint
  (s, f) <- get
  runLayer (workingdir s </> filename s) trans

grayscale = transform "grayscale" ["-type", "GrayScaleMatte"]

-- damn you, Prelude!
negateImage = transform "negate" ["-negate"]

-- damn you, Prelude!
flipImage = transform "flip" ["-flip"]

flopImage = transform "flop" ["-flop"]

crop geometry = transform ("crop_" ++ geometry) ["-crop", geometry]

format :: String -> Magick ()
format ext = do
  (s, f) <- get
  let path = replaceExtension (filename s) ext
  let s' = s {filename = path}
  let f' i = do i' <- f i
                rawConvert i' ["-", ext ++ ":-"]
  put (s', f')

resize :: String -> Magick ()
resize geometry = transform ("resize_" ++ geometry) ["-resize", geometry]

mask image = do
  transform "mask" [image, "-compose", "Copy_Opacity", "-composite"]

composite image = do
  transform "composite" [image, "-composite"]

compositeUnder image = do
  transform "composite_under" [image, "-compose", "DstOver", "-composite"]

merge image = do
  transform "merge" [image, "-background", "none", "-layers", "merge"]

mergeUnder image = do
  (s, f) <- get
  let s' = prepend ("mergeUnder-") s
  let f' i = do i' <- f i
                rawConvert i' [image, "-", "-background", "none", "-layers", "merge", "-"]
  put (s', f')

blend i m = do
  mask m
  compositeUnder i

makeUV r g = runLayer r $ do
  transform "uv" [g, "-background", "black", "-channel", "RG", "-combine"]

shadow color geometry = do
  s <- runClone $ do
    transform ("shadow_" ++ color ++ "_" ++ geometry) ["-background", color, "-shadow", geometry]
  mergeUnder s

focus m geometry = do
  m' <- runLayer m $ do
    negateImage
  blur m' geometry

blur m geometry = do
  transform "focus" [m, "-compose", "Blur", "-set", "option:compose:args", geometry, "-composite"]

level low high γ = do
  transform "level" ["-level", show low ++ "%," ++ show high ++ "%," ++ show γ]

gamma γ = level 0 100 γ

sigmoid factor threshold = do
  transform "sigmoid" ["-sigmoidal-contrast", show factor ++ "," ++ show threshold ++ "%"]

autoOrient = do
  transform "autoorient" ["-auto-orient"]

translate dx dy = do
  transform ("translate_" ++ (show dx) ++ "_" ++ (show dy)) ["-distort", "Affine", "0,0 " ++ (show dx) ++ "," ++ (show dy)]

getExif :: FilePath -> IO Exif
getExif filename = do
  i <- readImage filename
  rawExif <- rawIdentify i ["-verbose", "-"]
  -- split by lines, drop non-exif lines, then split "exif:key: value" into (key, value)
  let filtered = do l <- lines rawExif
                    let l' = dropWhile isSpace l
                    guard ("exif:" `isPrefixOf` l')
                    let l'' = drop (length "exif:") l'
                    let (key, value) = break (== ':') l''
                    return (key, drop 2 value)
  return $ formatExif (M.fromList filtered)

formatExif :: Map String String -> Exif
formatExif = M.mapWithKey $ \k v -> case k of
  "ExifImageLength" -> exifInt v
  "ExifImageWidth" -> exifInt v

  "Compression" -> exifInt v
  "Contrast" -> exifInt v

  "ExposureBiasValue" -> exifFrac v
  "ExposureMode" -> ES $ case v of "0" -> "Auto"
                                   "1" -> "Manual"
                                   "2" -> "Auto Bracket"
  "ExposureProgram" -> ES $ case v of "0" -> "Undefined"
                                      "1" -> "Manual"
                                      "2" -> "Normal"
                                      "3" -> "Aperture"
                                      "4" -> "Shutter"
                                      "5" -> "Creative"
                                      "6" -> "Action"
                                      "7" -> "Portrait"
                                      "8" -> "Landscape"
  "ExposureTime" -> exifFrac v
  "FNumber" -> exifFrac v

  "Flash" -> exifInt v

  "FocalLength" -> exifFrac v

  "ISOSpeedRatings" -> exifInt v

  "ShutterSpeedValue" -> exifFrac v
  "ApertureValue" -> exifFrac v
  "DigitalZoomRatio" -> exifFrac v

  "GainControl" -> ES $ case v of "0" -> "None"
                                  "1" -> "Low Up"
                                  "2" -> "High Up"
                                  "3" -> "Low Down"
                                  "4" -> "High Down"

  "LightSource" -> ES $ case v of "0" -> "Unknown"
                                  "1" -> "Daylight"
                                  "2" -> "Fluorescent"
                                  "3" -> "Tungsten (incandescent light)"
                                  "4" -> "Flash"
                                  "9" -> "Fine weather"
                                  "10" -> "Cloudy weather"
                                  "11" -> "Shade"
                                  "12" -> "Daylight fluorescent (D 5700 – 7100K)"
                                  "13" -> "Day white fluorescent (N 4600 – 5400K)"
                                  "14" -> "Cool white fluorescent (W 3900 – 4500K)"
                                  "15" -> "White fluorescent (WW 3200 – 3700K)"
                                  "17" -> "Standard light A"
                                  "18" -> "Standard light B"
                                  "19" -> "Standard light C"
                                  "20" -> "D55"
                                  "21" -> "D65"
                                  "22" -> "D75"
                                  "23" -> "D50"
                                  "24" -> "ISO studio tungsten"

  "Saturation" -> ES $ case v of "0" -> "Normal"
                                 "1" -> "Low Saturation"
                                 "2" -> "High Saturation"

  "Sharpness" -> ES $ case v of "0" -> "Normal"
                                "1" -> "Soft"
                                "2" -> "Hard"

  "SubjectDistance" -> exifFrac v
  "SubjectDistanceRange" -> ES $ case v of "0" -> "unknown"
                                           "1" -> "Macro"
                                           "2" -> "Close view"
                                           "3" -> "Distant view"

  "WhiteBalance" -> ES $ case v of "0" -> "Auto white balance"
                                   "1" -> "Manual white balance"

  "SceneCaptureType" -> ES $ case v of "0" -> "Standard"
                                       "1" -> "Landscape"
                                       "2" -> "Portrait"
                                       "3" -> "Night Scene"

  "SensingMethod" -> ES $ case v of "1" -> "Not defined"
                                    "2" -> "One-chip color area sensor"
                                    "3" -> "Two-chip color area sensor"
                                    "4" -> "Three-chip color area sensor"
                                    "5" -> "Color sequential area sensor"
                                    "7" -> "Trilinear sensor"
                                    "8" -> "Color sequential linear sensor"

  "MeteringMode" -> ES $ case v of "0" -> "Unknown"
                                   "1" -> "Average"
                                   "2" -> "CenterWeightedAverage"
                                   "3" -> "Spot"
                                   "4" -> "MultiSpot"
                                   "5" -> "Pattern"
                                   "6" -> "Partial"

  "XResolution" -> exifFrac v
  "YResolution" -> exifFrac v
  "ResolutionUnit" -> ES $ case v of "1" -> "None"
                                     "2" -> "Inch"
                                     "3" -> "cm"

  "CustomRendered" -> ES $ case v of "0" -> "Normal process"
                                     "1" -> "Custom process"

  "DateTimeDigitized" -> exifTime v
  "DateTimeOriginal" -> exifTime v
  "DateTime" -> exifTime v

  _ -> ES v

exifInt s = EI (read s)
exifFrac s = let (n, '/':d) = break (== '/') s in ED $ (read n / read d)
exifTime s = ET . utcTimeToPOSIXSeconds . readTime defaultTimeLocale "%Y:%m:%d %H:%M:%S" $ s

