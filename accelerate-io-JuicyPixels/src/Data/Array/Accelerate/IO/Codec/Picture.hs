{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Codec.Picture
-- Copyright   : [2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Codec.Picture
  where

import Data.Array.Accelerate                              hiding ( Vector )
import Data.Array.Accelerate.Array.Sugar                  ( EltRepr )
import Data.Array.Accelerate.IO.Codec.Picture.Types       () -- instances
import Data.Array.Accelerate.IO.Data.Vector.Storable

import Data.Vector.Storable                               ( Vector )
import Codec.Picture


-- | /O(1)/. Convert an Accelerate 'Array' into an 'Image'.
--
imageOfArray
    :: (Elt pixel, Vector (PixelBaseComponent pixel) ~ Vectors (EltRepr pixel))
    => Array DIM2 pixel
    -> Image pixel
imageOfArray arr =
  let Z :. imageHeight :. imageWidth = arrayShape arr
      imageData                      = toVectors arr
  in
  Image{..}

-- | /O(1)/. Convert an 'Image' into an Accelerate 'Array'.
--
arrayOfImage
    :: (Elt pixel, Vector (PixelBaseComponent pixel) ~ Vectors (EltRepr pixel))
    => Image pixel
    -> Array DIM2 pixel
arrayOfImage Image{..} =
  fromVectors (Z :. imageHeight :. imageWidth) imageData

