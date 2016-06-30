module Main where

import Color
import Lighting
import Object
import Ray
import Scene
import Trace

import Codec.Picture
import Codec.Picture.Types
import Data.List
import Data.Maybe
import Linear.V3
import Linear.Vector
import Linear.Metric
import System.IO

main = do
    scene <- sampleScene
    let aa = [(xo, yo) | xo <- [-0.4, 0, 0.4], yo <- [-0.4, 0, 0.4]]--[(xo, yo) | xo <- [-0.25, 0.25], yo <- [-0.25, 0.25]]
        render x y = antialiasedScreenPixel aa (fromIntegral x) (fromIntegral y) 900 600 sampleCamera scene sampleLights
    savePngImage "output.png" $ ImageRGBF (generateImage render 900 600)

antialiasedScreenPixel :: [(Float, Float)] -> Float -> Float -> Int -> Int -> Ray -> [Object] -> [Light] -> PixelRGBF
antialiasedScreenPixel offsets x y w h camera objects lights = mixcs (foldl1 addcc [(screenPixel (x + ox) (y + oy) w h camera objects lights) | (ox, oy) <- offsets]) (1.0 / fromIntegral (length offsets))
screenPixel :: Float -> Float -> Int -> Int -> Ray -> [Object] -> [Light] -> PixelRGBF
screenPixel x y w h camera objects lights = traceRay (screenRay camera x y (fromIntegral w) (fromIntegral h)) objects lights 2

screenRay :: Ray -> Float -> Float -> Float -> Float -> Ray
screenRay (Ray pos dir) x y w h =
    let xfov = (pi / 4) :: Float -- 90 degree horizontal fov
        yfov = atan ((tan xfov) * (h / w)) :: Float
        xFromMid = ((x / w)) - 0.5 :: Float
        yFromMid = ((y / h)) - 0.5 :: Float
        up = (V3 0 (-1) 0) :: V3 Float
        xAxis = normalize $ cross up dir :: V3 Float
        yAxis = normalize $ cross dir xAxis :: V3 Float
        xComponent = ((tan xfov) * xFromMid) *^ xAxis :: V3 Float
        yComponent = ((tan yfov) * yFromMid) *^ yAxis :: V3 Float
    in Ray pos (dir + xComponent + yComponent)
