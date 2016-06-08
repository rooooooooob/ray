module Main where

import Codec.Picture
import Data.List
import Data.Maybe
import Linear.V3
import Linear.Vector
import Linear.Metric
import System.Random

data Geometry = Sphere {
    center :: V3 Float,
    rad :: Float
}

data Ray = Ray {
    pos :: V3 Float,
    dir :: V3 Float
}

data Material = Material {
    col :: PixelRGBF,
    reflect :: Float,
    refract :: Float, -- refractive index!
    mdif :: Float,
    mspec :: Float,
    mshiny :: Float
}

data Object = Object {
    geo :: Geometry,
    mat :: Material
}

data Light = PointLight {
    lpos :: V3 Float,
    lrad :: Float,
    lcol :: PixelRGBF
} | DirectionalLight {
    ldir :: V3 Float,
    lcol :: PixelRGBF
}

instance Eq Object where
    x == y = (center.geo) x == (center.geo) y

main = savePngImage "output.png" $ ImageRGBF (generateImage (\x y -> antialiasedScreenPixel (zip [-0.4, 0, 0.4] [-0.4, 0, 0.4]) (fromIntegral x) (fromIntegral y) 1280 720) 1280 720)

antialiasedScreenPixel :: [(Float, Float)] -> Float -> Float -> Int -> Int -> PixelRGBF
antialiasedScreenPixel offsets x y w h = mixcs (foldl1 addcc [(screenPixel (x + ox) (y + oy) w h) | (ox, oy) <- offsets]) (1.0 / fromIntegral (length offsets))

screenPixel :: Float -> Float -> Int -> Int -> PixelRGBF
screenPixel x y w h = traceRay (screenRay x y (fromIntegral w) (fromIntegral h)) sampleScene sampleLights 3

sampleCamera = Ray (V3 0 (6) (7)) $ normalize (V3 0 (-0.6) (-0.7))
sampleScene = addRandomSpheres 32 (mkStdGen 42) (Material (PixelRGBF 0.5 0.55 0.7) 0.2 2 0.05 0.02 1337) $
    addRandomSpheres 32 (mkStdGen 23) (Material (PixelRGBF 0.5 0.75 1) 1 1 0.05 0.7 320)
    [ Object (Sphere (V3 0 0 0) 1.3) (Material (PixelRGBF 0.3 0.3 0.3) 0.3 1.5 0.005 0.2 2560)
    , Object (Sphere (V3 0.5 0.5 2) 0.8) (Material (PixelRGBF 1 0.2 0.2) 0.3 1 1 0 0)
    , Object (Sphere (V3 0.85 1.5 1) 0.3) (Material (PixelRGBF 0.2 1 0.2) 0.1 1 1 0 0)
    , Object (Sphere (V3 0 2 0) 0.4) (Material (PixelRGBF 0.2 0.2 1) 0.8 1 0.4 0 500)
    , Object (Sphere (V3 (-2) (-3) (-3)) 1) (Material (PixelRGBF 0.7 0.5 1) 0.5 1 0.1 3 600) ]
sampleLights = [ (PointLight (V3 (-5) 4 5.5) 6 (PixelRGBF 1 0.8 0.3))
    , (PointLight (V3 0 3.5 1.5) 3 (PixelRGBF 0.36 1.08 1.42))
    , (DirectionalLight (V3 0.2 (-1) 0) (PixelRGBF 0.5 0.5 0.5)) ]

addRandomSpheres :: RandomGen g => Int -> g -> Material -> [Object] -> [Object]
addRandomSpheres 0 rng mat objects = objects
addRandomSpheres n rng mat objects =
    let (x, rng2) = randomR (-2.0, 2.0) rng
        (y, rng3) = randomR (-2.0, 2.0) rng2
        (z, rng4) = randomR (-2.0, 2.0) rng3
        (r, rng5) = randomR (0.05, 0.25) rng4
    in addRandomSpheres (n - 1) rng5 mat $ (Object (Sphere (V3 x y z) r) mat):objects

screenRay :: Float -> Float -> Float -> Float -> Ray
screenRay x y w h = 
    let xfov = (pi / 4) :: Float -- 90 degree horizontal fov
        yfov = atan ((tan xfov) * (h / w)) :: Float
        xFromMid = ((x / w)) - 0.5 :: Float
        yFromMid = ((y / h)) - 0.5 :: Float
        up = (V3 0 (-1) 0) :: V3 Float
        xAxis = normalize (cross up (dir sampleCamera)) :: V3 Float
        yAxis = normalize (cross (dir sampleCamera) xAxis) :: V3 Float
        xComponent = ((tan xfov) * xFromMid) *^ xAxis :: V3 Float
        yComponent = ((tan yfov) * yFromMid) *^ yAxis :: V3 Float
    in Ray (pos sampleCamera) ((dir sampleCamera) + xComponent + yComponent)

mixcc :: PixelRGBF -> PixelRGBF -> PixelRGBF
mixcc (PixelRGBF ra ga ba) (PixelRGBF rb gb bb) = PixelRGBF (ra*rb) (ga*gb) (ba*bb)

mixcs :: PixelRGBF -> Float -> PixelRGBF
mixcs (PixelRGBF r g b) s = PixelRGBF (r*s) (g*s) (b*s)

addcc :: PixelRGBF -> PixelRGBF -> PixelRGBF
addcc (PixelRGBF ra ga ba) (PixelRGBF rb gb bb) = PixelRGBF (ra+rb) (ga+gb) (ba+bb)

intersectsObjects :: Ray -> [Object] -> Maybe (Float, Object)
intersectsObjects ray objects =
    let intersections = zip (map (intersects ray) objects) objects :: [(Maybe Float, Object)]
        collided = map (\(t, o) -> (fromJust t, o)) $ filter (\(t, obj) -> isJust t) intersections :: [(Float, Object)]
    in if null collided
       then Nothing
       else Just $ minimumBy (\(t1, o1) (t2, o2) -> compare t1 t2) collided

traceRay :: Ray -> [Object] -> [Light] -> Int -> PixelRGBF
traceRay ray objects lights depth = case intersectsObjects ray objects of
    Just (t, o) -> addcc (mixcs (col.mat $ o) 0.1) (onCollide ray ((pos ray) + ((dir ray) ^* t)) o objects lights depth)
    Nothing -> PixelRGBF 0 0 0

blackOr :: Bool -> PixelRGBF -> PixelRGBF
blackOr cond def = if cond then def else PixelRGBF 0 0 0

onCollide :: Ray -> V3 Float -> Object -> [Object] -> [Light] -> Int -> PixelRGBF
onCollide ray contactPos obj objects lights depth = 
    let base = foldl combineMaybeCols Nothing lights
        reflectRay = Ray (contactPos + 0.01 *^ snorm) $ normalize $ (dir ray) - (project snorm (dir ray)) ^* 2
        reflectCol = blackOr (depth > 0 && (reflect.mat $ obj) > 0) $ mixcs (traceRay reflectRay objects lights (depth - 1)) (reflect.mat $ obj)
        refractCol = blackOr (depth > 0 && (refract.mat $ obj) > 1) $ (refractTrace ray contactPos obj objects lights)
    in if isNothing base
       then PixelRGBF 0 0 0
       else addcc (addcc (fromJust base) reflectCol) refractCol
    where snorm = surfaceNorm (geo obj) contactPos :: V3 Float
          combineMaybeCols :: Maybe PixelRGBF -> Light -> Maybe PixelRGBF
          combineMaybeCols (Just acc) light =
              let res = lightContrib (-1*^(dir ray)) contactPos snorm obj objects light
              in Just $ if isJust res then addcc acc (fromJust res) else acc
          combineMaybeCols Nothing light = lightContrib (-1*^(dir ray)) contactPos snorm obj objects light

refractTrace :: Ray -> V3 Float -> Object -> [Object] -> [Light] -> PixelRGBF
refractTrace ray contactPos obj objects lights =
    let enterNorm = surfaceNorm (geo obj) contactPos :: V3 Float
        enterRay = Ray (contactPos - 0.01 *^ enterNorm) $ refractRay 1 (refract.mat $ obj) enterNorm ((-1) *^ (dir ray)) :: Ray
        exitPos = (pos enterRay) + ((dir enterRay) ^* (fromJust $ intersects enterRay obj)) :: V3 Float
        exitNorm = -1.0 *^ (surfaceNorm (geo obj) exitPos) :: V3 Float
        exitRay = Ray (exitPos - 0.01 *^exitNorm) $ refractRay (refract.mat $ obj) 1 exitNorm ((-1) *^ (dir enterRay)) :: Ray
    in traceRay exitRay objects lights 0
    where refractRay :: Float -> Float -> V3 Float -> V3 Float -> V3 Float
          refractRay n1 n2 snorm iray = ((n1 / n2)*(dot snorm iray) - (sqrt (1 - ((n1 / n2)*(n1 / n2))*(1 - (dot snorm iray)*(dot snorm iray)))))*^snorm - (n1 / n2)*^iray

lightContrib :: V3 Float -> V3 Float -> V3 Float -> Object -> [Object] -> Light -> Maybe PixelRGBF
lightContrib iray spos snorm obj objects (PointLight lpos lrad lcol) =
    let toLight = Ray (spos + 0.01 *^ snorm) $ normalize $ lpos - spos :: Ray
        hit = intersectsObjects toLight objects
        distToLight = distance lpos spos
        distContrib = 1 - (min 1 (max 0 (distToLight / lrad)))
        diffuse = mixcs (mixcc (col.mat $ obj) lcol) $ (mdif.mat $ obj)*(((max 0 $ dot snorm (dir toLight))) * distContrib)
        specular = mixcs (mixcc (col.mat $ obj) lcol) $ distContrib*(mspec.mat $ obj)*(max 0 $ dot snorm $ normalize $ iray + (dir toLight)) ** (mspec.mat $ obj)
    in if isJust hit
       then Nothing
       else Just $ addcc diffuse specular
lightContrib iray spos snorm obj objects (DirectionalLight ldir lcol) =
    let toLight = Ray (spos + 0.01 *^ snorm) $ normalize $ (-1) *^ ldir :: Ray
        hit = intersectsObjects toLight objects
    in if isJust hit
       then Nothing
       else Just $ mixcs (mixcc (col.mat $ obj) lcol) (max 0 $ dot snorm (dir toLight))

surfaceNorm :: Geometry -> V3  Float -> V3 Float
surfaceNorm (Sphere c r) p = normalize $ p - c

intersects :: Ray -> Object -> Maybe Float
intersects ray obj = intersectsImpl ray (geo obj)

-- returns Just t if the distance from the ray origin to the object is t, otherwise Nothing
intersectsImpl :: Ray -> Geometry -> Maybe Float
intersectsImpl (Ray p d) (Sphere c r) =
    let descrim = (dot d (p-c))*(dot d (p-c)) - (dot d d)*((dot (p-c) (p-c)) - r*r) :: Float
    in if descrim < 0.0 then Nothing else sphereIntersection
    where
        sphereIntersection :: Maybe Float
        sphereIntersection =
            let minusB = -dot d (p - c) :: Float
                denom = dot d d :: Float
                descrim = (dot d (p-c))*(dot d (p-c)) - (dot d d)*((dot (p-c) (p-c)) - r*r) :: Float
                descrimSqrt = sqrt descrim :: Float
                t1 = (minusB - descrimSqrt) / denom :: Float
                t2 = (minusB + descrimSqrt) / denom :: Float
                t = if min t1 t2 >= 0 then min t1 t2 else max t1 t2 :: Float
            in if t >= 0 then Just t else Nothing