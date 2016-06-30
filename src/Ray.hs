module Ray
( Ray(..)
) where

import Linear.V3
import Linear.Vector

data Ray = Ray {
    pos :: V3 Float,
    dir :: V3 Float
}