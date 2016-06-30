module Scene
( sampleCamera
, sampleScene
, sampleLights
) where

import Ray
import Lighting
import Object

import Codec.Picture
import Codec.Picture.Types
import Linear.Metric
import Linear.V3
import Linear.Vector
import System.Random

sampleCamera = Ray (V3 0 (4) (6)) $ normalize (V3 0 (-0.5) (-0.7))--Ray (0.6*^(V3 40 (15) (-70))) $ normalize (V3 (-0.4) (-0.15) (0.7))
sampleScene :: IO [Object]
sampleScene = do
    pyramidFile <- lines <$> readFile "pyramid.obj"
    (Right worldTex) <- readImage "earth.png"
    (Right moonTex) <- readImage "moon.jpg"
    (Right marsTex) <- readImage "mars.jpg"
    (Right blueTex) <- readImage "blue.jpg"
    return $ addRandomSpheres 32 (mkStdGen 42) (Material (solidColor $ PixelRGBF 0.5 0.55 0.7) 0 2 0.05 0.02 1337) $
           addRandomSpheres 32 (mkStdGen 23) (Material (textureMap $ toImageRGBF blueTex) 1 1 0.05 0.7 320)--solidColor $ PixelRGBF 0.5 0.75 1
           [ Object (Sphere (V3 (-0.5) (-0.5) 2) 0.8) (Material (solidColor $ PixelRGBF 0.3 0.3 0.3) 0.3 2 0.005 0.2 2560)
           , Object (Sphere (V3 0 0 0) 1.2) (Material (textureMap $ toImageRGBF worldTex) 1 1 1 0 0)--solidColor $ PixelRGBF 1 0.2 0.2
           , Object (Sphere (V3 0.85 1.5 1) 0.3) (Material (textureMap $ toImageRGBF marsTex) 0.1 1 1 0 0)--solidColor $ PixelRGBF 0.2 1 0.2
           , Object (Sphere (V3 0 2 0) 0.4) (Material (solidColor $ PixelRGBF 0.2 0.2 1) 0.8 1 0.4 0 500)
           , Object (Sphere (V3 (-2) (-3) (-3)) 1) (Material (textureMap $ toImageRGBF moonTex) 0.5 1 0.1 1 600) ]
           --, Object (loadObj pyramidFile) (Material (solidColor $ PixelRGBF 2 2 0) 0 1 1 0 0) ]
sampleLights = --[ (PointLight (V3 (-5) 4 5.5) 12 (PixelRGBF 4 2 0.1))
    --, (PointLight (V3 0 3.5 1.5) 5 (PixelRGBF 0.72 2.16 2.84))
    [ (DirectionalLight (V3 0 (-1) 0) (PixelRGBF 0.6 0.6 0.6)) ]

addRandomSpheres :: RandomGen g => Int -> g -> Material -> [Object] -> [Object]
addRandomSpheres 0 rng mat objects = objects
addRandomSpheres n rng mat objects =
    let (angle1, rng2) = randomR (0, 2*pi) rng
        (angle2, rng3) = randomR (0, 2*pi) rng2
        (dist, rng4) = randomR (1.6, 4) rng3
        x = dist*(cos angle1)
        y = dist*(sin angle1)
        z = dist*(sin angle2)
        (r, rng5) = randomR (0.05, 0.25) rng4
    in addRandomSpheres (n - 1) rng5 mat $ (Object (Sphere (V3 x y z) r) mat):objects