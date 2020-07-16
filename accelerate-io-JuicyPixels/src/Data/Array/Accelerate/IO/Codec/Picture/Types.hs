{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Codec.Picture.Types
-- Copyright   : [2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Codec.Picture.Types (

  Image(..),
  Pixel(PixelBaseComponent),
  Pixel8, Pixel16, Pixel32, PixelF,

  PixelYA8(..),     pattern PixelYA8_,
  PixelYA16(..),    pattern PixelYA16_,
  PixelRGB8(..),    pattern PixelRGB8_,
  PixelRGB16(..),   pattern PixelRGB16_,
  PixelRGBF(..),    pattern PixelRGBF_,
  PixelRGBA8(..),   pattern PixelRGBA8_,
  PixelRGBA16(..),  pattern PixelRGBA16_,
  PixelCMYK8(..),   pattern PixelCMYK8_,
  PixelCMYK16(..),  pattern PixelCMYK16_,
  PixelYCbCr8(..),  pattern PixelYCbCr8_,
  PixelYCbCrK8(..), pattern PixelYCbCrK8_,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type
import Data.Primitive.Vec

import Codec.Picture.Types


pattern PixelYA8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp PixelYA8
pattern PixelYA8_ y a = V2 y a

pattern PixelYA16_ :: Exp Pixel16 -> Exp Pixel16 -> Exp PixelYA16
pattern PixelYA16_ y a = V2 y a

pattern PixelRGB8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelRGB8
pattern PixelRGB8_ r g b = V3 r g b

pattern PixelRGB16_ :: Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp PixelRGB16
pattern PixelRGB16_ r g b = V3 r g b

pattern PixelRGBF_ :: Exp PixelF -> Exp PixelF -> Exp PixelF -> Exp PixelRGBF
pattern PixelRGBF_ r g b = V3 r g b

pattern PixelRGBA8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelRGBA8
pattern PixelRGBA8_ r g b a = V4 r g b a

pattern PixelRGBA16_ :: Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp PixelRGBA16
pattern PixelRGBA16_ r g b a = V4 r g b a

pattern PixelCMYK8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelCMYK8
pattern PixelCMYK8_ r g b a = V4 r g b a

pattern PixelCMYK16_ :: Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp PixelCMYK16
pattern PixelCMYK16_ r g b a = V4 r g b a

pattern PixelYCbCr8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelYCbCr8
pattern PixelYCbCr8_ y cb cr = V3 y cb cr

pattern PixelYCbCrK8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelYCbCrK8
pattern PixelYCbCrK8_ y cb cr k = V4 y cb cr k


instance Elt PixelYA8 where
  type EltR PixelYA8 = Vec2 Pixel8
  eltR                   = TupRsingle (VectorScalarType (VectorType 2 singleType))
  tagsR                  = [ TagRsingle (VectorScalarType (VectorType 2 singleType)) ]
  toElt (Vec2 y a)       = PixelYA8 y a
  fromElt (PixelYA8 y a) = Vec2 y a

instance Elt PixelYA16 where
  type EltR PixelYA16 = Vec2 Pixel16
  eltR                    = TupRsingle (VectorScalarType (VectorType 2 singleType))
  tagsR                   = [ TagRsingle (VectorScalarType (VectorType 2 singleType)) ]
  toElt (Vec2 y a)        = PixelYA16 y a
  fromElt (PixelYA16 y a) = Vec2 y a

instance Elt PixelRGB8 where
  type EltR PixelRGB8 = Vec3 Pixel8
  eltR                      = TupRsingle (VectorScalarType (VectorType 3 singleType))
  tagsR                     = [ TagRsingle (VectorScalarType (VectorType 3 singleType)) ]
  toElt (Vec3 r g b)        = PixelRGB8 r g b
  fromElt (PixelRGB8 r g b) = Vec3 r g b

instance Elt PixelRGB16 where
  type EltR PixelRGB16 = Vec3 Pixel16
  eltR                       = TupRsingle (VectorScalarType (VectorType 3 singleType))
  tagsR                      = [ TagRsingle (VectorScalarType (VectorType 3 singleType)) ]
  toElt (Vec3 r g b)         = PixelRGB16 r g b
  fromElt (PixelRGB16 r g b) = Vec3 r g b

instance Elt PixelRGBF where
  type EltR PixelRGBF = Vec3 PixelF
  eltR                      = TupRsingle (VectorScalarType (VectorType 3 singleType))
  tagsR                     = [ TagRsingle (VectorScalarType (VectorType 3 singleType)) ]
  toElt (Vec3 r g b)        = PixelRGBF r g b
  fromElt (PixelRGBF r g b) = Vec3 r g b

instance Elt PixelRGBA8 where
  type EltR PixelRGBA8 = Vec4 Pixel8
  eltR                         = TupRsingle (VectorScalarType (VectorType 4 singleType))
  tagsR                        = [ TagRsingle (VectorScalarType (VectorType 4 singleType)) ]
  toElt (Vec4 r g b a)         = PixelRGBA8 r g b a
  fromElt (PixelRGBA8 r g b a) = Vec4 r g b a

instance Elt PixelRGBA16 where
  type EltR PixelRGBA16 = Vec4 Pixel16
  eltR                          = TupRsingle (VectorScalarType (VectorType 4 singleType))
  tagsR                         = [ TagRsingle (VectorScalarType (VectorType 4 singleType)) ]
  toElt (Vec4 r g b a)          = PixelRGBA16 r g b a
  fromElt (PixelRGBA16 r g b a) = Vec4 r g b a

instance Elt PixelCMYK8 where
  type EltR PixelCMYK8 = Vec4 Pixel8
  eltR                         = TupRsingle (VectorScalarType (VectorType 4 singleType))
  tagsR                        = [ TagRsingle (VectorScalarType (VectorType 4 singleType)) ]
  toElt (Vec4 c m y k)         = PixelCMYK8 c m y k
  fromElt (PixelCMYK8 c m y k) = Vec4 c m y k

instance Elt PixelCMYK16 where
  type EltR PixelCMYK16 = Vec4 Pixel16
  eltR                          = TupRsingle (VectorScalarType (VectorType 4 singleType))
  tagsR                         = [ TagRsingle (VectorScalarType (VectorType 4 singleType)) ]
  toElt (Vec4 c m y k)          = PixelCMYK16 c m y k
  fromElt (PixelCMYK16 c m y k) = Vec4 c m y k

instance Elt PixelYCbCr8 where
  type EltR PixelYCbCr8 = Vec3 Pixel8
  eltR                          = TupRsingle (VectorScalarType (VectorType 3 singleType))
  tagsR                         = [ TagRsingle (VectorScalarType (VectorType 3 singleType)) ]
  toElt (Vec3 y cb cr)          = PixelYCbCr8 y cb cr
  fromElt (PixelYCbCr8 y cb cr) = Vec3 y cb cr

instance Elt PixelYCbCrK8 where
  type EltR PixelYCbCrK8 = Vec4 Pixel8
  eltR                             = TupRsingle (VectorScalarType (VectorType 4 singleType))
  tagsR                            = [ TagRsingle (VectorScalarType (VectorType 4 singleType)) ]
  toElt (Vec4 y cb cr k)           = PixelYCbCrK8 y cb cr k
  fromElt (PixelYCbCrK8 y cb cr k) = Vec4 y cb cr k

