{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Codec.BMP
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Read and write uncompressed 24 or 32-bit Windows BMP images into
-- a packed-word RGBA format. See the /colour-accelerate/ package for colour
-- representations and utilities such as packing and unpacking.
--

module Data.Array.Accelerate.IO.Codec.BMP (

  RGBA32,
  readImageFromBMP, writeImageToBMP,

) where

import Data.Word
import Codec.BMP

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.IO.Data.ByteString


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
          bs    = unpackBMPToRGBA32 bmp'
          arr   = fromByteStrings (Z :. h :. w) bs
          --
          bmp'  = bmp { bmpBitmapInfo = info' }
          info' = case bmpBitmapInfo bmp of
                    InfoV3 i -> InfoV3 (info3 i)
                    InfoV4 i -> InfoV4 (info4 i)
                    InfoV5 i -> InfoV5 (info5 i)

          info3 BitmapInfoV3{..} = BitmapInfoV3 { dib3HeightFlipped = not dib3HeightFlipped, .. }
          info4 BitmapInfoV4{..} = BitmapInfoV4 { dib4InfoV3 = info3 dib4InfoV3, .. }
          info5 BitmapInfoV5{..} = BitmapInfoV5 { dib5InfoV4 = info4 dib5InfoV4, .. }
      --
      return $ Right arr


-- | Write the image data to a file.
--
writeImageToBMP :: FilePath -> Array DIM2 RGBA32 -> IO ()
writeImageToBMP file rgba = writeBMP file bmp'
  where
    Z :. h :. w = shape rgba
    bs          = toByteStrings rgba
    bmp         = packRGBA32ToBMP w h bs
    --
    bmp'        = bmp { bmpBitmapInfo = info' }
    info'       = case bmpBitmapInfo bmp of
                    InfoV3 i -> InfoV3 (info3 i)
                    InfoV4 i -> InfoV4 (info4 i)
                    InfoV5 i -> InfoV5 (info5 i)

    info3 BitmapInfoV3{..} = BitmapInfoV3 { dib3Height = -dib3Height, dib3HeightFlipped = True, .. }
    info4 BitmapInfoV4{..} = BitmapInfoV4 { dib4InfoV3 = info3 dib4InfoV3, .. }
    info5 BitmapInfoV5{..} = BitmapInfoV5 { dib5InfoV4 = info4 dib5InfoV4, .. }

