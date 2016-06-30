module Object
( Geometry(..)
, Material(..)
, Object(..)
, loadObj
, solidColor
, textureMap
, toImageRGBF
, intersectsObjects
, intersects
) where

import Color
import Ray

import Codec.Picture
import Codec.Picture.Types
import Data.List
import Data.List.Split
import Data.Ord
import Data.Maybe
import Control.Monad
import Linear.Metric
import Linear.V3
import Linear.Vector

import Debug.Trace

data Geometry = Sphere {
    center :: V3 Float,
    rad :: Float
} | Triangle {
    a,  b, c :: V3 Float
} | Mesh {
    vertices :: [V3 Float],
    faces :: [[Int]]
}

data Material = Material {
    col :: Float -> Float -> PixelRGBF,
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

loadObj :: [String] -> Geometry
loadObj fileLines = readObjLine fileLines (Mesh [] [])
    where readObjLine (x:xs) (Mesh verts faces) = case (words x) !! 0 of
              "v" -> readObjLine xs $ Mesh (verts ++ [V3 (read $ (words x) !! 1) (read $ (words x) !! 2) (read $ (words x) !! 3)]) faces
              "f" -> let newface = map ((+(-1)) . read . head . (splitOn "/")) $ drop 1 (words x) :: [Int]
                     in readObjLine xs $ Mesh verts (faces ++ [newface])
              _ -> readObjLine xs (Mesh verts faces)
          readObjLine [] mesh = id mesh

solidColor :: PixelRGBF -> Float -> Float -> PixelRGBF
solidColor c _ _ = c

textureMap :: Image PixelRGBF -> Float -> Float -> PixelRGBF
textureMap tex u v = if u >= 0 && u < 1 && v >= 0 && v < 1
    then pixelAt tex (floor ((1 - u) *(fromIntegral$imageWidth tex))) (floor ((1 - v) * (fromIntegral$imageHeight tex)))
    else trace ("invalid u-v coordinates: u = " ++ (show u) ++ "; v = " ++ (show v)) black

toImageRGBF :: DynamicImage -> Image PixelRGBF
toImageRGBF (ImageRGB8 img)    = promoteImage img
toImageRGBF (ImageRGBF img)    = img
toImageRGBF (ImageYCbCr8 img)  = promoteImage $ (convertImage img :: Image PixelRGB8)

intersectsObjects :: Ray -> [Object] -> Maybe (Float, V3 Float, Float, Float, Object)
intersectsObjects ray objects =
    let fst5 (x, _, _, _, _) = x
        intersection :: Object -> Maybe (Float, V3 Float, Float, Float, Object)
        intersection obj = case intersects ray $ geo obj of
            Just (t, n, u, v) -> Just (t, n, u, v, obj)
            Nothing -> Nothing
        intersections = mapMaybe intersection objects
    in if not $ null intersections
       then Just $ minimumBy (comparing fst5) intersections
       else Nothing

sign x = if x >= 0 then 1 else -1
-- returns Just t if the distance from the ray origin to the object is t, otherwise Nothing
intersects :: Ray -> Geometry -> Maybe (Float, V3 Float, Float, Float)
intersects (Ray o d) (Sphere c r) =
    let minusB = -dot d (o - c) :: Float
        denom = dot d d :: Float
        descrim = (dot d (o-c))*(dot d (o-c)) - (dot d d)*((dot (o-c) (o-c)) - r*r) :: Float
        descrimSqrt = sqrt descrim :: Float
        t1 = (minusB - descrimSqrt) / denom :: Float
        t2 = (minusB + descrimSqrt) / denom :: Float
        t = if min t1 t2 >= 0 then min t1 t2 else max t1 t2 :: Float
        (V3 dx dy dz) = normalize $ c - (o + d^*t)
        u = 0.5 + (atan2 dz dx)/(2*pi)
        v = 0.5 - (asin dy)/pi
    in if descrim < 0 || t < 0
       then Nothing
       else Just (t, normalize $ (o + d^*t) - c, u, v)
intersects (Ray o d) (Triangle a b c) =
    let ab = (b - a)
        ac = (c - a)
        n = if dot d (cross ab ac) > 0 then cross ac ab else cross ab ac
        planetoo = project n (o - a) :: V3 Float
        costheta = dot (normalize $ (-1.0)*^planetoo) (normalize d) :: Float
        t = (norm planetoo) / (costheta) :: Float
        p = o + (t*^d)
        atest = dot n (cross (b - a) (p - a))
        btest = dot n (cross (c - b) (p - b))
        ctest = dot n (cross (a - c) (p - c))
    in if abs (dot n d) < 0.0001 || t < 0 || sign atest /= sign btest || sign atest /= sign ctest
       then Nothing
       else Just (t, normalize n, 0, 0)--todo add in triangle u-v
intersects ray (Mesh verts faces) =
    let anyTri (a:b:c:rest) = case intersects ray (Triangle (verts !! a) (verts !! b) (verts !! c)) of
            Just hit -> Just hit
            Nothing -> anyTri (a:c:rest)
        anyTri _ = Nothing
        anyFace (f:rest) = case anyTri f of
            Just hit -> Just hit
            Nothing -> anyFace rest
        anyFace [] = Nothing
    in anyFace faces