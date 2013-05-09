-- |
-- Module      : Data.Array.Accelerate.IO.BMP
-- Copyright   : [2012] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.BMP (

  -- ** Bitmap images
  --
  -- | Reading and writing arrays as uncompressed 24 or 32-bit Windows BMP
  -- files.
  --
  RGBA32,
  readImageFromBMP, writeImageToBMP,

  -- *** Manipulating pixels
  unpackRGBA32, packRGBA32, luminanceOfRGBA32, rgba32OfLuminance, rgba32OfFloat,

) where

import Data.Bits
import Data.Word
import Codec.BMP

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO.ByteString      as A


-- File IO ---------------------------------------------------------------------

-- | Read RGBA components from a BMP file.
--
readImageFromBMP :: FilePath -> IO (Either Error (Array DIM2 RGBA32))
readImageFromBMP file = do
  ebmp          <- readBMP file
  case ebmp of
    Left err    -> return $ Left err
    Right bmp   -> do
      let (w,h) = bmpDimensions bmp
          bs    = unpackBMPToRGBA32 bmp
      --
      Right `fmap` A.fromByteString (Z :. h :. w) ((), bs)


-- | Write the image data to a file.
--
writeImageToBMP :: FilePath -> Array DIM2 RGBA32 -> IO ()
writeImageToBMP file rgba = do
  let Z :. h :. w       =  A.arrayShape rgba
  ((), bs)              <- A.toByteString rgba
  --
  writeBMP file (packRGBA32ToBMP w h bs)


-- Manipulating pixels ---------------------------------------------------------
--
-- TLM: perhaps this should be moved into something like:
--      accelerate-algorithms:Data.Array.Accelerate.Algorithms.Pixel
--

-- | Packed RGBA pixel data
--
type RGBA32 = Word32

-- | Unpack a 'RGBA32' value into a tuple of (Red, Green, Blue, Alpha) values.
--
unpackRGBA32 :: Exp RGBA32 -> Exp (Word8, Word8, Word8, Word8)
unpackRGBA32 rgba =
  let r = A.fromIntegral $ rgba                   .&. 0xFF
      g = A.fromIntegral $ (rgba `div` 0x100)     .&. 0xFF
      b = A.fromIntegral $ (rgba `div` 0x10000)   .&. 0xFF
      a = A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF
  in
  lift (r, g, b, a)


-- | Promote a tuple of (Red, Green, Blue, Alpha) values into a packed 'RGBA32'
-- value.
--
packRGBA32 :: Exp (Word8, Word8, Word8, Word8) -> Exp RGBA32
packRGBA32 rgba =
  let (r', g', b', a')  = unlift rgba
      r                 = A.fromIntegral r'
      g                 = (A.fromIntegral g') * 0x100
      b                 = (A.fromIntegral b') * 0x10000
      a                 = (A.fromIntegral a') * 0x1000000
  in
  r + g + b + a


-- | Convert an RGBA colour to its luminance value in the range [0..1].
--
luminanceOfRGBA32 :: (Elt a, IsFloating a) => Exp RGBA32 -> Exp a
luminanceOfRGBA32 rgba =
  let r = 0.3  * A.fromIntegral (rgba                 .&. 0xFF)
      g = 0.59 * A.fromIntegral ((rgba `div` 0x100)   .&. 0xFF)
      b = 0.11 * A.fromIntegral ((rgba `div` 0x10000) .&. 0xFF)
  in
  (r + g + b) / 255


-- | Convert a value in the range [0..1] to a grey RGB colour.
--
rgba32OfLuminance :: (Elt a, IsFloating a) => Exp a -> Exp RGBA32
rgba32OfLuminance val =
  let v = A.truncate (255 * val) -- (0 `A.max` val `A.min` 1)
      r = v
      g = v * 0x100
      b = v * 0x10000
      a =     0xFF000000
  in
  r + g + b + a


-- | Promote a tuple of (Red, Green, Blue, Alpha) values in the range [0..1]
-- into a packed 'RGBA32'.
--
rgba32OfFloat :: (Elt a, IsFloating a) => Exp (a, a, a, a) -> Exp RGBA32
rgba32OfFloat rgba =
  let (r, g, b, a)      = unlift rgba
      r'                = A.truncate (255 * r) * 0x1000000
      g'                = A.truncate (255 * g) * 0x10000
      b'                = A.truncate (255 * b) * 0x100
      a'                = A.truncate (255 * a)
  in
  r' + g' + b' + a'

