{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.BMP
-- Copyright   : [2012..2014] Trevor L. McDonell
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

-- | Unpack a (little-endian) 'RGBA32' value into a tuple of (Red, Green, Blue,
-- Alpha) values.
--
unpackRGBA32, unpackRGBA32le, unpackRGBA32be
    :: Exp RGBA32
    -> Exp (Word8, Word8, Word8, Word8)
unpackRGBA32 = unpackRGBA32le

unpackRGBA32le rgba =
  let a = A.fromIntegral (rgba `A.shiftR` 24)
      b = A.fromIntegral (rgba `A.shiftR` 16)
      g = A.fromIntegral (rgba `A.shiftR` 8)
      r = A.fromIntegral rgba
  in
  lift (r, g, b, a)

unpackRGBA32be rgba =
  let r = A.fromIntegral (rgba `A.shiftR` 24)
      g = A.fromIntegral (rgba `A.shiftR` 16)
      b = A.fromIntegral (rgba `A.shiftR` 8)
      a = A.fromIntegral rgba
  in
  lift (r, g, b, a)


-- | Promote a tuple of (Red, Green, Blue, Alpha) values into a packed
-- (little-endian) 'RGBA32' value.
--
packRGBA32, packRGBA32le, packRGBA32be
    :: Exp (Word8, Word8, Word8, Word8)
    -> Exp RGBA32
packRGBA32 = packRGBA32le

packRGBA32le (unlift -> (r, g, b, a))
   =  A.fromIntegral a `A.shiftL` 24
  .|. A.fromIntegral b `A.shiftL` 16
  .|. A.fromIntegral g `A.shiftL` 8
  .|. A.fromIntegral r

packRGBA32be (unlift -> (r, g, b, a))
   =  A.fromIntegral r `A.shiftL` 24
  .|. A.fromIntegral g `A.shiftL` 16
  .|. A.fromIntegral b `A.shiftL` 8
  .|. A.fromIntegral a


-- | Convert an RGBA colour to its luminance value in the range [0..1].
--
luminanceOfRGBA32 :: (Elt a, IsFloating a) => Exp RGBA32 -> Exp a
luminanceOfRGBA32 (unlift . unpackRGBA32 -> (r, g, b, _a :: Exp Word8)) =
  let r' = 0.3  * A.fromIntegral r
      g' = 0.59 * A.fromIntegral g
      b' = 0.11 * A.fromIntegral b
  in
  (r' + g' + b') / 255


-- | Convert a value in the range [0..1] to a grey RGB colour.
--
rgba32OfLuminance :: (Elt a, IsFloating a) => Exp a -> Exp RGBA32
rgba32OfLuminance val =
  let v = A.truncate (255 * val) -- (0 `A.max` val `A.min` 1)
  in
  packRGBA32 (lift (v, v, v, constant 0xFF))


-- | Promote a tuple of (Red, Green, Blue, Alpha) values in the range [0..1]
-- into a packed 'RGBA32'.
--
rgba32OfFloat :: (Elt a, IsFloating a) => Exp (a, a, a, a) -> Exp RGBA32
rgba32OfFloat (unlift -> (r,g,b,a)) =
  let r' = A.truncate (255 * r)
      g' = A.truncate (255 * g)
      b' = A.truncate (255 * b)
      a' = A.truncate (255 * a)
  in
  packRGBA32 (lift (r', g', b', a'))

