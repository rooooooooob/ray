module Trace
( traceRay
) where

import Color
import Collision
import Lighting
import Object
import Ray

import Codec.Picture
import Codec.Picture.Types
import Data.Maybe
import Linear.Metric
import Linear.V3
import Linear.Vector

-- Traces a ray through the given scene
traceRay :: Ray       -- ^ The ray to trace
         -> [Object]  -- ^ The objects in the scene
         -> [Light]   -- ^ The lights in the scene
         -> Int       -- ^ The recursion depth for reflections/refractions (none at 0)
         -> PixelRGBF -- ^ The colour at the position of the traced ray
traceRay ray objects lights depth = case intersectsObjects ray objects of
    Just (t, n, u, v, o) -> addcc (mixcs ((col $ mat o) u v) 0.15) (onCollide ray ((pos ray) + ((dir ray) ^* t)) n u v o objects lights depth)
    Nothing -> black

-- assuming a collision with the input Object, gives the colour at the collided location
onCollide :: CollisionInfo -- ^ The collision that happened
          -> [Object]      -- ^ The other objects in the scene
          -> [Light]       -- ^ The lights in the scene
          -> Int           -- ^ The reflection recursion depth (none at 0)
          -> PixelRGBF     -- ^ The colour at the point of collision found (subject to textures, lights, etc)
onCollide colInfo  objects lights depth = base `addcc` reflectCol `addcc` refractCol
    where contactPos = colPos colInfo
          snorm = colSurfNorm colInfo
          obj = colobj colInfo
          tcol = (col $ mat obj) (colU colInfo) (colV colInfo)
          base = foldl (\c light -> addcc c $ lightContrib (-1*^(dir ray)) contactPos snorm tcol obj objects light) black lights
          reflectRay = Ray (contactPos + 0.0001 *^ snorm) $ normalize $ (dir ray) - (project snorm (dir ray)) ^* 2
          reflectCol = if depth > 0 && (reflect.mat $ obj) > 0
                          then mixcs (traceRay reflectRay objects lights (depth - 1)) (reflect.mat $ obj)
                          else black
          refractCol = if depth > 0 && (refract.mat $ obj) > 1
                          then refractTrace ray contactPos snorm obj objects lights
                          else black

-- casts a refraction ray (ie glass, water, etc) through an object (note: assumes closed mesh)
refractTrace :: CollisionInfo -- ^ The collision that we need to refract out from
             -> [Object]      -- ^ The other objects in the scene
             -> [Light]       -- ^ The lights in the scene
             -> PixelRGBF     -- ^ The colour found from refracting through the object
refractTrace colInfo objects lights = traceRay exitRay objects lights 0
    where obj = colObj colInfo
          enterNorm = colSurfNorm colInfo
          enterRay = Ray ((colPos colInfo) - 0.0001 *^ enterNorm) $ refractRay 1 (refract.mat $ obj) enterNorm ((-1) *^ (dir ray)) :: Ray
          (exitT, exitNorm, _, _) = fromJust $ intersects enterRay (geo obj)
          exitPos = (pos enterRay) + (dir enterRay) ^* exitT
          exitRay = Ray (exitPos + 0.0001 *^exitNorm) $ refractRay (refract.mat $ obj) 1 (-1*^exitNorm) ((-1) *^ (dir enterRay)) :: Ray 
          refractRay :: Float -> Float -> V3 Float -> V3 Float -> V3 Float
          refractRay n1 n2 snorm iray = ((n1 / n2)*(dot snorm iray) - (sqrt (1 - ((n1 / n2)*(n1 / n2))*(1 - (dot snorm iray)*(dot snorm iray)))))*^snorm - (n1 / n2)*^iray
