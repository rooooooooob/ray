module Lighting
( Light(..)
, lightContrib
) where

import Color
import Object
import Ray

import Codec.Picture
import Data.Maybe
import Linear.Metric
import Linear.V3
import Linear.Vector

data Light = PointLight {
    lpos :: V3 Float,
    lrad :: Float,
    lcol :: PixelRGBF
} | DirectionalLight {
    ldir :: V3 Float,
    lcol :: PixelRGBF
}

lightContrib :: V3 Float -> V3 Float -> V3 Float -> PixelRGBF -> Object -> [Object] -> Light -> PixelRGBF
lightContrib iray spos snorm tcol obj objects light = 
    let (toLight, intensity, dist) = case light of
            (PointLight lpos lrad lcol) ->
                let distToLight = distance lpos spos
                    distContrib = 1 - (min 1 $ distToLight / lrad)
                in (normalize $ lpos - spos, distContrib, distToLight)
            (DirectionalLight ldir lcol) -> (normalize $ (-1) *^ ldir, 1, read "Infinity")
        diffuse lcol = mixcs (mixcc tcol lcol) $ (mdif.mat $ obj)*(max 0 $ dot snorm toLight)
        specular lcol = mixcs (mixcc tcol lcol) $ (mspec.mat $ obj)*(max 0 $ dot snorm $ normalize $ iray + toLight) ** (mspec.mat $ obj)
    in if isNothing $ intersectsObjects (Ray (spos + 0.0001 *^ snorm) toLight) objects
       then mixcs (addcc (diffuse $ lcol light) (specular $ lcol light)) intensity
       else black