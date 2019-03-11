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

module Data.Array.Accelerate.IO.Codec.Picture.Types
  where

import Data.Array.Accelerate
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Codec.Picture.Types


pattern PixelYA8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp PixelYA8
pattern PixelYA8_ y a = Pattern (y, a)

pattern PixelYA16_ :: Exp Pixel16 -> Exp Pixel16 -> Exp PixelYA16
pattern PixelYA16_ y a = Pattern (y, a)

pattern PixelRGB8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelRGB8
pattern PixelRGB8_ r g b = Pattern (r, g, b)

pattern PixelRGB16_ :: Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp PixelRGB16
pattern PixelRGB16_ r g b = Pattern (r, g, b)

pattern PixelRGBF_ :: Exp PixelF -> Exp PixelF -> Exp PixelF -> Exp PixelRGBF
pattern PixelRGBF_ r g b = Pattern (r, g, b)

pattern PixelRGBA8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelRGBA8
pattern PixelRGBA8_ r g b a = Pattern (r, g, b, a)

pattern PixelRGBA16_ :: Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp PixelRGBA16
pattern PixelRGBA16_ r g b a = Pattern (r, g, b, a)

pattern PixelCMYK8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelCMYK8
pattern PixelCMYK8_ r g b a = Pattern (r, g, b, a)

pattern PixelCMYK16_ :: Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp Pixel16 -> Exp PixelCMYK16
pattern PixelCMYK16_ r g b a = Pattern (r, g, b, a)

pattern PixelYCbCr8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelYCbCr8
pattern PixelYCbCr8_ y cb cr = Pattern (y, cb, cr)

pattern PixelYCbCrK8_ :: Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp Pixel8 -> Exp PixelYCbCrK8
pattern PixelYCbCrK8_ y cb cr k = Pattern (y, cb, cr, k)


instance Elt PixelYA8 where
  type EltRepr PixelYA8 = V2 Pixel8
  eltType                = TypeRscalar scalarType
  toElt (V2 y a)         = PixelYA8 y a
  fromElt (PixelYA8 y a) = V2 y a

instance Elt PixelYA16 where
  type EltRepr PixelYA16 = V2 Pixel16
  eltType                 = TypeRscalar scalarType
  toElt (V2 y a)          = PixelYA16 y a
  fromElt (PixelYA16 y a) = V2 y a

instance Elt PixelRGB8 where
  type EltRepr PixelRGB8 = V3 Pixel8
  eltType                   = TypeRscalar scalarType
  toElt (V3 r g b)          = PixelRGB8 r g b
  fromElt (PixelRGB8 r g b) = V3 r g b

instance Elt PixelRGB16 where
  type EltRepr PixelRGB16 = V3 Pixel16
  eltType                    = TypeRscalar scalarType
  toElt (V3 r g b)           = PixelRGB16 r g b
  fromElt (PixelRGB16 r g b) = V3 r g b

instance Elt PixelRGBF where
  type EltRepr PixelRGBF = V3 PixelF
  eltType                   = TypeRscalar scalarType
  toElt (V3 r g b)          = PixelRGBF r g b
  fromElt (PixelRGBF r g b) = V3 r g b

instance Elt PixelRGBA8 where
  type EltRepr PixelRGBA8 = V4 Pixel8
  eltType                      = TypeRscalar scalarType
  toElt (V4 r g b a)           = PixelRGBA8 r g b a
  fromElt (PixelRGBA8 r g b a) = V4 r g b a

instance Elt PixelRGBA16 where
  type EltRepr PixelRGBA16 = V4 Pixel16
  eltType                       = TypeRscalar scalarType
  toElt (V4 r g b a)            = PixelRGBA16 r g b a
  fromElt (PixelRGBA16 r g b a) = V4 r g b a

instance Elt PixelCMYK8 where
  type EltRepr PixelCMYK8 = V4 Pixel8
  eltType                      = TypeRscalar scalarType
  toElt (V4 c m y k)           = PixelCMYK8 c m y k
  fromElt (PixelCMYK8 c m y k) = V4 c m y k

instance Elt PixelCMYK16 where
  type EltRepr PixelCMYK16 = V4 Pixel16
  eltType                       = TypeRscalar scalarType
  toElt (V4 c m y k)            = PixelCMYK16 c m y k
  fromElt (PixelCMYK16 c m y k) = V4 c m y k

instance Elt PixelYCbCr8 where
  type EltRepr PixelYCbCr8 = V3 Pixel8
  eltType                       = TypeRscalar scalarType
  toElt (V3 y cb cr)            = PixelYCbCr8 y cb cr
  fromElt (PixelYCbCr8 y cb cr) = V3 y cb cr

instance Elt PixelYCbCrK8 where
  type EltRepr PixelYCbCrK8 = V4 Pixel8
  eltType                          = TypeRscalar scalarType
  toElt (V4 y cb cr k)             = PixelYCbCrK8 y cb cr k
  fromElt (PixelYCbCrK8 y cb cr k) = V4 y cb cr k


instance IsProduct Elt PixelYA8 where
  type ProdRepr PixelYA8 = ProdRepr (Pixel8, Pixel8)
  fromProd (PixelYA8 y a) = fromProd @Elt (y,a)
  toProd p                = let (y,a) = toProd @Elt p in PixelYA8 y a
  prod                    = prod @Elt @(Pixel8, Pixel8)

instance IsProduct Elt PixelYA16 where
  type ProdRepr PixelYA16 = ProdRepr (Pixel16, Pixel16)
  fromProd (PixelYA16 y a) = fromProd @Elt (y,a)
  toProd p                 = let (y,a) = toProd @Elt p in PixelYA16 y a
  prod                     = prod @Elt @(Pixel16, Pixel16)

instance IsProduct Elt PixelRGB8 where
  type ProdRepr PixelRGB8 = ProdRepr (Pixel8, Pixel8, Pixel8)
  fromProd (PixelRGB8 r g b) = fromProd @Elt (r,g,b)
  toProd p                   = let (r,g,b) = toProd @Elt p in PixelRGB8 r g b
  prod                       = prod @Elt @(Pixel8, Pixel8, Pixel8)

instance IsProduct Elt PixelRGB16 where
  type ProdRepr PixelRGB16 = ProdRepr (Pixel16, Pixel16, Pixel16)
  fromProd (PixelRGB16 r g b) = fromProd @Elt (r,g,b)
  toProd p                    = let (r,g,b) = toProd @Elt p in PixelRGB16 r g b
  prod                        = prod @Elt @(Pixel16, Pixel16, Pixel16)

instance IsProduct Elt PixelRGBF where
  type ProdRepr PixelRGBF = ProdRepr (PixelF, PixelF, PixelF)
  fromProd (PixelRGBF r g b) = fromProd @Elt (r,g,b)
  toProd p                   = let (r,g,b) = toProd @Elt p in PixelRGBF r g b
  prod                       = prod @Elt @(PixelF, PixelF, PixelF)

instance IsProduct Elt PixelRGBA8 where
  type ProdRepr PixelRGBA8 = ProdRepr (Pixel8, Pixel8, Pixel8, Pixel8)
  fromProd (PixelRGBA8 r g b a) = fromProd @Elt (r,g,b,a)
  toProd p                      = let (r,g,b,a) = toProd @Elt p in PixelRGBA8 r g b a
  prod                          = prod @Elt @(Pixel8, Pixel8, Pixel8, Pixel8)

instance IsProduct Elt PixelRGBA16 where
  type ProdRepr PixelRGBA16 = ProdRepr (Pixel16, Pixel16, Pixel16, Pixel16)
  fromProd (PixelRGBA16 r g b a) = fromProd @Elt (r,g,b,a)
  toProd p                       = let (r,g,b,a) = toProd @Elt p in PixelRGBA16 r g b a
  prod                           = prod @Elt @(Pixel16, Pixel16, Pixel16, Pixel16)

instance IsProduct Elt PixelCMYK8 where
  type ProdRepr PixelCMYK8 = ProdRepr (Pixel8, Pixel8, Pixel8, Pixel8)
  fromProd (PixelCMYK8 c m y k) = fromProd @Elt (c,m,y,k)
  toProd p                      = let (c,m,y,k) = toProd @Elt p in PixelCMYK8 c m y k
  prod                          = prod @Elt @(Pixel8, Pixel8, Pixel8, Pixel8)

instance IsProduct Elt PixelCMYK16 where
  type ProdRepr PixelCMYK16 = ProdRepr (Pixel16, Pixel16, Pixel16, Pixel16)
  fromProd (PixelCMYK16 c m y k) = fromProd @Elt (c,m,y,k)
  toProd p                       = let (c,m,y,k) = toProd @Elt p in PixelCMYK16 c m y k
  prod                           = prod @Elt @(Pixel16, Pixel16, Pixel16, Pixel16)

instance IsProduct Elt PixelYCbCr8 where
  type ProdRepr PixelYCbCr8 = ProdRepr (Pixel8, Pixel8, Pixel8)
  fromProd (PixelYCbCr8 y cb cr) = fromProd @Elt (y,cb,cr)
  toProd p                       = let (y,cb,cr) = toProd @Elt p in PixelYCbCr8 y cb cr
  prod                           = prod @Elt @(Pixel8, Pixel8, Pixel8)

instance IsProduct Elt PixelYCbCrK8 where
  type ProdRepr PixelYCbCrK8 = ProdRepr (Pixel8, Pixel8, Pixel8, Pixel8)
  fromProd (PixelYCbCrK8 y cb cr k) = fromProd @Elt (y,cb,cr,k)
  toProd p                          = let (y,cb,cr,k) = toProd @Elt p in PixelYCbCrK8 y cb cr k
  prod                              = prod @Elt @(Pixel8, Pixel8, Pixel8, Pixel8)

