module Main where

import Codec.Picture
import Data.Maybe
import Linear.V3
import Linear.Vector
import Linear.Metric

data Geometry = Sphere {
    center :: V3 Float,
    rad :: Float
}

data Ray = Ray {
    pos :: V3 Float,
    dir :: V3 Float
}

main = savePngImage "output.png" $ ImageRGBF (generateImage (\x y -> screenPixel x y 640 480) 640 480)

screenPixel :: Int -> Int -> Int -> Int -> PixelRGBF
screenPixel x y w h = traceRay (screenRay (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)) sampleScene

sampleCamera = Ray (V3 0 24 26) $ normalize (V3 0 (-0.9) (-1))

screenRay :: Float -> Float -> Float -> Float -> Ray
screenRay x y w h = 
    let xfov = pi / 2 :: Float -- 90 degree horizontal fov
        yfov = atan ((tan xfov) * (h / w)) :: Float
        xFromMid = ((x / w)) - 0.5 :: Float
        yFromMid = ((y / h)) - 0.5 :: Float
        up = (V3 0 1 0) :: V3 Float
        xAxis = normalize (cross up (dir sampleCamera)) :: V3 Float
        yAxis = normalize (cross (dir sampleCamera) xAxis) :: V3 Float
        xComponent = ((tan xfov) * xFromMid) *^ xAxis :: V3 Float
        yComponent = ((tan yfov) * yFromMid) *^ yAxis :: V3 Float
    in Ray (pos sampleCamera) ((xComponent + yComponent) - ((pos sampleCamera) + (dir sampleCamera)))

sampleScene = [Sphere (V3 0 0 0) 0.013]

traceRay :: Ray -> [Geometry] -> PixelRGBF
traceRay ray geometry = 
    let intersections = mapMaybe (intersects ray) geometry
    in if null intersections then PixelRGBF 0 0 0 else PixelRGBF 0 0 1

-- returns Just t if the distance from the ray origin to the object is t, otherwise Nothing
intersects :: Ray -> Geometry -> Maybe Float
intersects (Ray p d) (Sphere c r) =
    let descrim = (dot d (p-c))*(dot d (p-c)) - (dot d d)*(dot (p-c) (p-c)) - r*r :: Float
    in if descrim < 0.0 then Just sphereIntersection else Nothing
    where
        sphereIntersection :: Float
        sphereIntersection =
            let minusB = dot (-1.0 *^ d) (p - c) :: Float
                denom = dot d d :: Float
                descrim = (dot d (p-c))*(dot d (p-c)) - (dot d d)*(dot (p-c) (p-c)) - r*r :: Float
                descrimSqrt = sqrt(descrim) :: Float
                t1 = (minusB - descrimSqrt) / denom :: Float
                t2 = (minusB + descrimSqrt) / denom :: Float
                t = min t1 t2 :: Float
            in t