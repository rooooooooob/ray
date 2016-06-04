module Main where

import Codec.Picture
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

main = savePngImage "output.png" $ ImageRGBF (generateImage blackPixel 640 480)

blackPixel :: Int -> Int -> PixelRGBF
blackPixel x y = PixelRGBF  0 1 0

screenPixel :: Float -> Float -> Float -> Float -> PixelRGBF
screenPixel x y w h = trace (screenRay x y w h) sampleScene

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

sampleScene = [Sphere (V3 0 0 0) 1.3]

trace :: Ray -> [Geometry] -> PixelRGBF
trace ray geometry = PixelRGBF 0 0 0