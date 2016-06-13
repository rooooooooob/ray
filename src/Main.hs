module Main where
import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Linear.V3
import Linear.Vector
import Linear.Metric
import System.Random
import System.IO
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
data Ray = Ray {
    pos :: V3 Float,
    dir :: V3 Float
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
data Light = PointLight {
    lpos :: V3 Float,
    lrad :: Float,
    lcol :: PixelRGBF
} | DirectionalLight {
    ldir :: V3 Float,
    lcol :: PixelRGBF
}

solidColor :: PixelRGBF -> Float -> Float -> PixelRGBF
solidColor c _ _ = c

textureMap :: Image PixelRGBF -> Float -> Float -> PixelRGBF
textureMap tex u v = if u >= 0 && u < 1 && v >= 0 && v < 1
    then pixelAt tex (floor ((1 - u) *(fromIntegral$imageWidth tex))) (floor ((1 - v) * (fromIntegral$imageHeight tex)))
    else trace ("invalid u-v coordinates: u = " ++ (show u) ++ "; v = " ++ (show v)) black
toImageRGBF :: DynamicImage -> Image PixelRGBF
toImageRGBF (ImageRGB8 img)    = promoteImage img
--toImageRGBF (ImageRGB16 img)   = promoteImage img
toImageRGBF (ImageRGBF img)    = img
toImageRGBF (ImageYCbCr8 img)  = promoteImage $ (convertImage img :: Image PixelRGB8)

black = PixelRGBF 0 0 0

main = do
    scene <- sampleScene
    let aa = [(xo, yo) | xo <- [-0.25, 0.25], yo <- [-0.25, 0.25]]--[(xo, yo) | xo <- [-0.4, 0, 0.4], yo <- [-0.4, 0, 0.4]]
        render x y = antialiasedScreenPixel aa (fromIntegral x) (fromIntegral y) 1920 1080 sampleCamera scene sampleLights
    savePngImage "output.png" $ ImageRGBF (generateImage render 1920 1080)

antialiasedScreenPixel :: [(Float, Float)] -> Float -> Float -> Int -> Int -> Ray -> [Object] -> [Light] -> PixelRGBF
antialiasedScreenPixel offsets x y w h camera objects lights = mixcs (foldl1 addcc [(screenPixel (x + ox) (y + oy) w h camera objects lights) | (ox, oy) <- offsets]) (1.0 / fromIntegral (length offsets))
screenPixel :: Float -> Float -> Int -> Int -> Ray -> [Object] -> [Light] -> PixelRGBF
screenPixel x y w h camera objects lights = traceRay (screenRay camera x y (fromIntegral w) (fromIntegral h)) objects lights 2
derp x y = PixelRGBF ((200 - x - y)/200) 0 ((x + y)/200)
sampleCamera = Ray (V3 0 (5) (7)) $ normalize (V3 0 (-0.5) (-0.7))--Ray (0.6*^(V3 40 (15) (-70))) $ normalize (V3 (-0.4) (-0.15) (0.7))
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
sampleLights = --[ (PointLight (V3 (-5) 4 5.5) 6 (PixelRGBF 1 0.8 0.3))
    --, (PointLight (V3 0 3.5 1.5) 3 (PixelRGBF 0.36 1.08 1.42))
    [ (DirectionalLight (V3 0 (-1) 0) (PixelRGBF 1 1 1)) ]

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

mixcc :: PixelRGBF -> PixelRGBF -> PixelRGBF
mixcc (PixelRGBF ra ga ba) (PixelRGBF rb gb bb) = PixelRGBF (ra*rb) (ga*gb) (ba*bb)

mixcs :: PixelRGBF -> Float -> PixelRGBF
mixcs (PixelRGBF r g b) s = PixelRGBF (r*s) (g*s) (b*s)

addcc :: PixelRGBF -> PixelRGBF -> PixelRGBF
addcc (PixelRGBF ra ga ba) (PixelRGBF rb gb bb) = PixelRGBF (ra+rb) (ga+gb) (ba+bb)

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

traceRay :: Ray -> [Object] -> [Light] -> Int -> PixelRGBF
traceRay ray objects lights depth = case intersectsObjects ray objects of
    Just (t, n, u, v, o) -> addcc (mixcs ((col $ mat o) u v) 0.1) (onCollide ray ((pos ray) + ((dir ray) ^* t)) n u v o objects lights depth)
    Nothing -> black

onCollide :: Ray -> V3 Float -> V3 Float -> Float -> Float -> Object -> [Object] -> [Light] -> Int -> PixelRGBF
onCollide ray contactPos snorm u v obj objects lights depth = base `addcc` reflectCol `addcc` refractCol
    where tcol = (col $ mat obj) u v
          base = foldl (\c light -> addcc c $ lightContrib (-1*^(dir ray)) contactPos snorm tcol obj objects light) black lights
          reflectRay = Ray (contactPos + 0.0001 *^ snorm) $ normalize $ (dir ray) - (project snorm (dir ray)) ^* 2
          reflectCol = if depth > 0 && (reflect.mat $ obj) > 0
                          then mixcs (traceRay reflectRay objects lights (depth - 1)) (reflect.mat $ obj)
                          else black
          refractCol = if depth > 0 && (refract.mat $ obj) > 1
                          then refractTrace ray contactPos snorm obj objects lights
                          else black

refractTrace :: Ray -> V3 Float -> V3 Float -> Object -> [Object] -> [Light] -> PixelRGBF
refractTrace ray contactPos enterNorm obj objects lights = traceRay exitRay objects lights 0
    where enterRay = Ray (contactPos - 0.0001 *^ enterNorm) $ refractRay 1 (refract.mat $ obj) enterNorm ((-1) *^ (dir ray)) :: Ray
          (exitT, exitNorm, _, _) = fromJust $ intersects enterRay (geo obj)
          exitPos = (pos enterRay) + (dir enterRay) ^* exitT
          exitRay = Ray (exitPos + 0.0001 *^exitNorm) $ refractRay (refract.mat $ obj) 1 (-1*^exitNorm) ((-1) *^ (dir enterRay)) :: Ray 
          refractRay :: Float -> Float -> V3 Float -> V3 Float -> V3 Float
          refractRay n1 n2 snorm iray = ((n1 / n2)*(dot snorm iray) - (sqrt (1 - ((n1 / n2)*(n1 / n2))*(1 - (dot snorm iray)*(dot snorm iray)))))*^snorm - (n1 / n2)*^iray

lightContrib :: V3 Float -> V3 Float -> V3 Float -> PixelRGBF -> Object -> [Object] -> Light -> PixelRGBF
lightContrib iray spos snorm tcol obj objects light = 
    let (toLight, intensity, dist) = case light of
            (PointLight lpos lrad lcol) ->
                let distToLight = distance lpos spos
                    distContrib = 1 - (min 1 $ distToLight / lrad)
                in (normalize $ lpos - spos, distContrib, distToLight)
            (DirectionalLight ldir lcol) -> (normalize $ (-1) *^ ldir, 1, read "Infinity")
        diffuse lcol = mixcs (mixcc tcol lcol) $ (mdif.mat $ obj)*(max 0 $ dot snorm toLight)
        specular lcol = mixcs (mixcc tcol lcol) $ (mspec.mat $ obj)*(max 0 $ dot snorm $ normalize $ iray + toLight) ** (mspec.mat $ obj)
    in if isNothing $ intersectsObjects (Ray (spos + 0.0001 *^ snorm) toLight) objects
       then mixcs (addcc (diffuse $ lcol light) (specular $ lcol light)) intensity
       else black
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

loadObj :: [String] -> Geometry
loadObj fileLines = readObjLine fileLines (Mesh [] [])
    where readObjLine (x:xs) (Mesh verts faces) = case (words x) !! 0 of
              "v" -> readObjLine xs $ Mesh (verts ++ [V3 (read $ (words x) !! 1) (read $ (words x) !! 2) (read $ (words x) !! 3)]) faces
              "f" -> let newface = map ((+(-1)) . read . head . (splitOn "/")) $ drop 1 (words x) :: [Int]
                     in readObjLine xs $ Mesh verts (faces ++ [newface])
              _ -> readObjLine xs (Mesh verts faces)
          readObjLine [] mesh = id mesh