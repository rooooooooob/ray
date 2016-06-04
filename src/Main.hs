module Main where

import Codec.Picture

main = savePngImage "output.png" $ ImageRGBF (generateImage blackPixel 640 480)

blackPixel :: Int -> Int -> PixelRGBF
blackPixel x y = PixelRGBF  0 1 0

--screenPixel :: Int -> Int -> Pixel
--screenPixel x y  = trace x y 

--trace :: Int -> Int -> Int -> Int -> [Sphere] -> Pixel
