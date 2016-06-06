module Main where
import Codec.Picture
import Data.List
import Data.Maybe
import Linear.V3
import Linear.Vector
import Linear.Metric
import Debug.Trace
data Geometry = Sphere {
    center :: V3 Float,
    rad :: Float
}
data Ray = Ray {
    pos :: V3 Float,
    dir :: V3 Float
}
data Material = Material {
    col :: PixelRGBF
}
data Object = Object {
    geo :: Geometry,
    mat :: Material
}
data Light = PointLight {
    lpos :: V3 Float,
    lrad :: Float,
    lcol :: PixelRGBF
}
instance Eq Object where
    x == y = (center.geo) x == (center.geo) y
main = savePngImage "output.png" $ ImageRGBF (generateImage (\x y -> screenPixel x y 1280 720) 1280 720)

screenPixel :: Int -> Int -> Int -> Int -> PixelRGBF
screenPixel x y w h = traceRay (screenRay (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)) sampleScene (PointLight (V3 0 5 0) 9 (PixelRGBF 1 1 1)) --(PixelRGBF 0.18 1.08 1.62))

sampleCamera = Ray (V3 0 6 7) $ normalize (V3 0 (-0.6) (-0.7))

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

sampleScene = [Object (Sphere (V3 0 0 0) 1.3) (Material (PixelRGBF 0.4 0.5 0.6)), Object (Sphere (V3 0.5 0.5 2) 0.8) (Material (PixelRGBF 1 0.2 0.2)), Object (Sphere (V3 0.85 1.5 1) 0.3) (Material (PixelRGBF 0.2 1 0.2)), Object (Sphere (V3 0 2 0) 0.4) (Material (PixelRGBF 0.2 0.2 1))]

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

traceRay :: Ray -> [Object] -> Light -> PixelRGBF
traceRay ray objects lights = 
    let hit = intersectsObjects ray objects
    in if isJust hit
       then let (t, o) = fromJust hit
            in addcc (mixcs (col.mat $ o) 0.05) (onCollide ray ((pos ray) + ((dir ray) ^* t)) o objects lights)
       else PixelRGBF 0 0 0

onCollide :: Ray -> V3 Float -> Object -> [Object] -> Light -> PixelRGBF
onCollide ray contactPos obj objects lights = 
    let snorm = surfaceNorm (geo obj) contactPos :: V3 Float
        toLight = Ray (contactPos + 0.01 *^ snorm) $ normalize $ (lpos lights) - contactPos :: Ray
        hit = intersectsObjects toLight objects
        distToLight = distance (lpos lights) contactPos
        distContrib = 1 - (min 1 (max 0 (distToLight / (lrad lights))))
    in if isJust hit 
        then PixelRGBF 0 0 0
        else mixcs (mixcc (col.mat $ obj) (lcol lights)) (((max 0 $ dot snorm (dir toLight))) * distContrib) 

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
            let minusB = dot (-1.0 *^ d) (p - c) :: Float
                denom = (dot d d) :: Float
                descrim = (dot d (p-c))*(dot d (p-c)) - (dot d d)*((dot (p-c) (p-c)) - r*r) :: Float
                descrimSqrt = sqrt(descrim) :: Float
                t1 = (minusB - descrimSqrt) / denom :: Float
                t2 = (minusB + descrimSqrt) / denom :: Float
                t = if min t1 t2 >= 0 then min t1 t2 else max t1 t2 :: Float
            in if t >= 0 then Just t else Nothing