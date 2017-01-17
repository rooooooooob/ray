module Color
( black
, mixcc
, mixcs
, addcc
, toSRGB
, fromSRGB
) where

import Codec.Picture
import Codec.Picture.Types


black = PixelRGBF 0 0 0

-- Multiplies two colours together component-wise
mixcc :: PixelRGBF -> PixelRGBF -> PixelRGBF
mixcc (PixelRGBF ra ga ba) (PixelRGBF rb gb bb) = PixelRGBF (ra*rb) (ga*gb) (ba*bb)

-- Multiplies a colour by a scalar
mixcs :: PixelRGBF -> Float -> PixelRGBF
mixcs (PixelRGBF r g b) s = PixelRGBF (r*s) (g*s) (b*s)

-- Adds two colours together component-wise
addcc :: PixelRGBF -> PixelRGBF -> PixelRGBF
addcc (PixelRGBF ra ga ba) (PixelRGBF rb gb bb) = PixelRGBF (ra+rb) (ga+gb) (ba+bb)

-- note: sRGB here is approximates by x^2.2 gamma since it's close enough

-- converts from an sRGB encoded pixel to one in linear gamma space
fromSRGB :: PixelRGBF -> PixelRGBF
fromSRGB (PixelRGBF r g b) = PixelRGBF (r**2.2) (g**2.2) (b**2.2)

-- converts from a linear gamma space pixel to sRGB encoding
toSRGB :: PixelRGBF -> PixelRGBF
toSRGB (PixelRGBF r g b) = PixelRGBF (r**(1.0/2.2)) (g**(1.0/2.2)) (b**(1.0/2.2))