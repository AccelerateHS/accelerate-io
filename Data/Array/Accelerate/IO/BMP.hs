-- |
-- Module      : Data.Array.Accelerate.IO.BMP
-- Copyright   : [2012..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Read and write BMP images into a packed-word RGBA format. See the
-- /colour-accelerate/ package for colour representations and utilities such as
-- packing and unpacking.
--

module Data.Array.Accelerate.IO.BMP (

  -- ** Bitmap images
  --
  -- | Reading and writing arrays as uncompressed 24 or 32-bit Windows BMP
  -- files.
  --
  RGBA32,
  readImageFromBMP, writeImageToBMP,

) where

import Data.Word
import Codec.BMP

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO.ByteString      as A


-- | Packed RGBA pixel data
--
type RGBA32 = Word32

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
      Right `fmap` A.fromByteString (Z :. h :. w) bs


-- | Write the image data to a file.
--
writeImageToBMP :: FilePath -> Array DIM2 RGBA32 -> IO ()
writeImageToBMP file rgba = do
  let Z :. h :. w       =  A.arrayShape rgba
  bs                    <- A.toByteString rgba
  --
  writeBMP file (packRGBA32ToBMP w h bs)

