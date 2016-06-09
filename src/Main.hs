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
} | Triangle {
    a, b, c :: V3 Float
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

black = PixelRGBF 0 0 0
--[(xo, yo) | xo <- [-0.4, 0, 0.4], yo <- [-0.4, 0, 0.4]]
main = savePngImage "output.png" $ ImageRGBF (generateImage (\x y -> screenPixel (fromIntegral x) (fromIntegral y) 1280 720) 1280 720)

antialiasedScreenPixel :: [(Float, Float)] -> Float -> Float -> Int -> Int -> PixelRGBF
antialiasedScreenPixel offsets x y w h = mixcs (foldl1 addcc [(screenPixel (x + ox) (y + oy) w h) | (ox, oy) <- offsets]) (1.0 / fromIntegral (length offsets))

screenPixel :: Float -> Float -> Int -> Int -> PixelRGBF
screenPixel x y w h = traceRay (screenRay x y (fromIntegral w) (fromIntegral h)) sampleScene sampleLights 1

sampleCamera = (V3 0 (6) (7)) $ normalize (V3 0 (-0.6) (-0.7))
sampleScene = addRandomSpheres 32 (mkStdGen 42) (Material (PixelRGBF 0.5 0.55 0.7) 0 2 0.05 0.02 1337) $
    addRandomSpheres 32 (mkStdGen 23) (Material (PixelRGBF 0.5 0.75 1) 1 1 0.05 0.7 320)
    [ Object (Sphere (V3 0 0 0) 1.3) (Material (PixelRGBF 0.3 0.3 0.3) 0.3 1.5 0.005 0.2 2560)
    , Object (Sphere (V3 0.5 0.5 2) 0.8) (Material (PixelRGBF 1 0.2 0.2) 0.3 1 1 0 0)
    , Object (Sphere (V3 0.85 1.5 1) 0.3) (Material (PixelRGBF 0.2 1 0.2) 0.1 1 1 0 0)
    , Object (Sphere (V3 0 2 0) 0.4) (Material (PixelRGBF 0.2 0.2 1) 0.8 1 0.4 0 500)
    , Object (Sphere (V3 (-2) (-3) (-3)) 1) (Material (PixelRGBF 0.7 0.5 1) 0.5 1 0.1 3 600)
    , Object (Triangle (V3 (-2) 0 2) (V3 2 0 2) (V3 0 2 0)) (Material (PixelRGBF 1 0.8 0.2) 1 2 1 0 0) ]
    --, Object (Triangle (V3 (-200) (-20) 200) (V3 (-200) (-20) (-200)) (V3 (-200) (-20) 200)) (Material (PixelRGBF 0.3 1 1) 1 1 1 0 0)
    --, Object (Triangle (V3 200 (-20) 200) (V3 (-200) (-20) 200) (V3 200 (-20) (-200))) (Material (PixelRGBF 1 0.3 1) 1 1 1 0 0) ]
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

intersectsObjects :: Ray -> [Object] -> Maybe (Float, V3 Float, Object)
intersectsObjects ray objects =
    let intersections = mapMaybe (intersects ray) objects :: [(Float, V3 Float, Object)]
    in if null intersections
       then Nothing
       else Just $ minimumBy (\(t1, n1, o1) (t2, n2, o2) -> compare t1 t2) intersections

traceRay :: Ray -> [Object] -> [Light] -> Int -> PixelRGBF
traceRay ray objects lights depth = case intersectsObjects ray objects of
    Just (t, n, o) -> addcc (mixcs (col.mat $ o) 0.1) (onCollide ray ((pos ray) + ((dir ray) ^* t)) n o objects lights depth)
    Nothing -> black

onCollide :: Ray -> V3 Float -> V3 Float -> Object -> [Object] -> [Light] -> Int -> PixelRGBF
onCollide ray contactPos snorm obj objects lights depth = base `addcc` reflectCol `addcc` refractCol
    where base = foldl (\c light -> addcc c $ lightContrib (-1*^(dir ray)) contactPos snorm obj objects light) black lights
          reflectRay = Ray (contactPos + 0.01 *^ snorm) $ normalize $ (dir ray) - (project snorm (dir ray)) ^* 2
          reflectCol = if depth > 0 && (reflect.mat $ obj) > 0
                          then mixcs (traceRay reflectRay objects lights (depth - 1)) (reflect.mat $ obj)
                          else black
          refractCol = if depth > 0 && (refract.mat $ obj) > 1
                          then refractTrace ray contactPos snorm obj objects lights
                          else black

refractTrace :: Ray -> V3 Float -> V3 Float -> Object -> [Object] -> [Light] -> PixelRGBF
refractTrace ray contactPos enterNorm obj objects lights = traceRay exitRay objects lights 0
    where enterRay = Ray (contactPos - 0.01 *^ enterNorm) $ refractRay 1 (refract.mat $ obj) enterNorm ((-1) *^ (dir ray)) :: Ray
          (exitT, exitNorm, _) = fromJust $ intersects enterRay obj
          exitPos = (pos enterRay) + (dir enterRay) ^* exitT
          exitRay = Ray (exitPos + 0.01 *^exitNorm) $ refractRay (refract.mat $ obj) 1 (-1*^exitNorm) ((-1) *^ (dir enterRay)) :: Ray 
          refractRay :: Float -> Float -> V3 Float -> V3 Float -> V3 Float
          refractRay n1 n2 snorm iray = ((n1 / n2)*(dot snorm iray) - (sqrt (1 - ((n1 / n2)*(n1 / n2))*(1 - (dot snorm iray)*(dot snorm iray)))))*^snorm - (n1 / n2)*^iray

lightContrib :: V3 Float -> V3 Float -> V3 Float -> Object -> [Object] -> Light -> PixelRGBF
lightContrib iray spos snorm obj objects light = 
    let (toLight, intensity, dist) = case light of
            (PointLight lpos lrad lcol) ->
                let distToLight = distance lpos spos
                    distContrib = 1 - (min 1 $ distToLight / lrad)
                in (normalize $ lpos - spos, distContrib, distToLight)
            (DirectionalLight ldir lcol) -> (normalize $ (-1) *^ ldir, 1, read "Infinity")
        diffuse lcol = mixcs (mixcc (col.mat $ obj) lcol) $ (mdif.mat $ obj)*(max 0 $ dot snorm toLight)
        specular lcol = mixcs (mixcc (col.mat $ obj) lcol) $ (mspec.mat $ obj)*(max 0 $ dot snorm $ normalize $ iray + toLight) ** (mspec.mat $ obj)
    in if isNothing $ intersectsObjects (Ray (spos + 0.01 *^ snorm) toLight) objects
       then mixcs (addcc (diffuse $ lcol light) (specular $ lcol light)) intensity
       else black

-- returns Just t if the distance from the ray origin to the object is t, otherwise Nothing
intersects :: Ray -> Object -> Maybe (Float, V3 Float, Object)
intersects (Ray o d) obj = case geo obj of
    (Sphere c r) ->
        let minusB = -dot d (o - c) :: Float
            denom = dot d d :: Float
            descrim = (dot d (o-c))*(dot d (o-c)) - (dot d d)*((dot (o-c) (o-c)) - r*r) :: Float
            descrimSqrt = sqrt descrim :: Float
            t1 = (minusB - descrimSqrt) / denom :: Float
            t2 = (minusB + descrimSqrt) / denom :: Float
            t = if min t1 t2 >= 0 then min t1 t2 else max t1 t2 :: Float
        in if descrim < 0 || t < 0
           then Nothing
           else Just (t, normalize $ (o + d^*t) - c, obj)
    (Triangle a b c) ->
        let ab = (b - a)
            ac = (c - a)
            n = cross ab ac
            ndrd = dot n d :: Float
            t = ((dot n o) + (dot a n))/ndrd :: Float
            p = o + t*^d
            atest = cross (b - a) (p - a) 
            btest = cross (c - b) (p - b) 
            ctest = cross (a - c) (p - c)
        in if abs ndrd < 0.01 || t < 0 || (dot n atest) < 0 || (dot n btest) < 0 || (dot n ctest) < 0--) && (isNothing $ intersects (Ray p d) (Object (Sphere a 0.3) (Material (PixelRGBF 1 0 0) 0.3 1.5 0.005 0.2 2560))) && (isNothing $ intersects (Ray p d) (Object (Sphere b 0.3) (Material (PixelRGBF 0.3 0.3 0.3) 0.3 1.5 0.005 0.2 2560))) && (isNothing $ intersects (Ray p d) (Object (Sphere c 0.3) (Material (PixelRGBF 0.3 0.3 0.3) 0.3 1.5 0.005 0.2 2560)))
           then Nothing
           else Just (t, normalize n, obj)