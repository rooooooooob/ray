module Object
( Geometry(..)
, Material(..)
, Object(..)
, loadObj
, solidColor
, textureMap
, toImageRGBF
) where

import Color
import Ray

import Codec.Picture
import Codec.Picture.Types
import Data.List.Split
import Data.Maybe
import Linear.V3
import Linear.Vector

import Debug.Trace

data Geometry = Sphere {
    center :: V3 Float, -- ^ The center of the sphere
    rad :: Float        -- ^ The radius of the sphere
} | Triangle {
    a,  b, c :: V3 Float -- ^ The 3 corners of the triangle
} | Mesh {
    vertices :: [V3 Float], -- ^ List of all vertices of the polygon mesh
    faces :: [[Int]]        -- ^  List of faces. Faces consist of the INDICES of 3 or more vertices
}

-- a u-v to colour mapping
type TextureMap = Float -> Float -> PixelRGBF

data Material = Material {
    col :: TextureMap, -- ^ the colour u-v map over the material
    reflect :: Float,  -- ^ scalar amount to multiply reflection colour by
    refract :: Float,  -- ^ refractive index
    mdif :: Float,     -- ^ scalar amount for the diffuse lighting
    mspec :: Float,    -- ^ specular coefficient to raise by
    mshiny :: Float    -- ^ scalar amount for the specular lighting
}

data Object = Object {
    geo :: Geometry, -- ^ The geometry of the object, used for collisions
    mat :: Material  -- ^ Information about the material of the object such as texture, reflectiveness, etc
}

-- Loads a OBJ format model from the input file
loadObj :: [String] -> Geometry
loadObj fileLines = readObjLine fileLines (Mesh [] [])
    where readObjLine (x:xs) (Mesh verts faces) = case (words x) !! 0 of
              "v" -> readObjLine xs $ Mesh (verts ++ [V3 (read $ (words x) !! 1) (read $ (words x) !! 2) (read $ (words x) !! 3)]) faces
              "f" -> let newface = map ((+(-1)) . read . head . (splitOn "/")) $ drop 1 (words x) :: [Int]
                     in readObjLine xs $ Mesh verts (faces ++ [newface])
              _ -> readObjLine xs (Mesh verts faces)
          readObjLine [] mesh = id mesh

-- u-v map for a uniform-colour texture
solidColor :: PixelRGBF -> TextureMap
solidColor c = \u v -> fromSRGB c

-- u-v map for an image texture
textureMap :: Image PixelRGBF -> TextureMap
textureMap tex = \u v -> if u >= 0 && u < 1 && v >= 0 && v < 1
    then fromSRGB $ pixelAt tex (floor ((1 - u) *(fromIntegral$imageWidth tex))) (floor ((1 - v) * (fromIntegral$imageHeight tex)))
    else trace ("invalid u-v coordinates: u = " ++ (show u) ++ "; v = " ++ (show v)) black

-- Converts a DynamicImage (that you would get from loading a file) to a PixelRGBF one we can work with
toImageRGBF :: DynamicImage -> Image PixelRGBF
toImageRGBF (ImageRGB8 img)    = promoteImage img
toImageRGBF (ImageRGBF img)    = img
toImageRGBF (ImageYCbCr8 img)  = promoteImage $ (convertImage img :: Image PixelRGB8)