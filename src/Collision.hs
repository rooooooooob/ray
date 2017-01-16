module Collision
( CollisionInfo(..)
, colPos
, intersectsObjects
, intersects
) where

import Object
import Ray

import Codec.Picture
import Codec.Picture.Types
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
import Linear.Metric
import Linear.V3
import Linear.Vector

data CollisionInfo = CollisionInfo {
    colDist :: Float,        -- ^ Scalar distance from the ray origin to the collision point   
    colSurfNorm :: V3 Float, -- ^ Surface normal at the collision point       
    colU:: Float,            -- ^ u-coordinate (0-1) at the collision point (for textures)
    colV :: Float,           -- ^ v-coordinate (0-1) at the collision point (for textures)
    colObj :: Object,        -- ^ The object that was collided with
    colRay :: Ray            -- ^ The ray that collided with the object
}

-- gets the point of collision of a collision
colPos :: CollisionInfo -> V3 Float
colPos colInfo = (pos.colRay $ colInfo) + ((dir.colRay $ colInfo) ^* (colDist colInfo))

-- Finds all intersections of a ray through a scene of objects
intersectsObjects :: Ray                 -- ^ The ray to test collision for
                  -> [Object]            -- ^ All the objects to test collision with
                  -> Maybe CollisionInfo -- ^ Collision information about which object was hit and where, Nothing if no collisions were found
intersectsObjects ray objects =
    let intersection :: Object -> Maybe CollisionInfo
        intersection obj = case intersects ray $ geo obj of
            Just (t, n, u, v) -> Just $ CollisionInfo t n u v obj ray
            Nothing -> Nothing
        intersections = mapMaybe intersection objects
    in if not $ null intersections
       then Just $ minimumBy (comparing colDist) intersections
       else Nothing

-- Tests intersection against a single object
intersects :: Ray           -- ^ The ray to test collision for
           -> Geometry      -- ^ The geometry of of the object to test against
           -> Maybe (Float, V3 Float, Float, Float) -- ^ Collision information about which object was hit and where, Nothing if no collisions were found
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
        sign x = if x >= 0 then 1 else -1
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