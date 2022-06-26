module Ray where

import Vec

--   Ray = origin    direction
data Ray = Ray { o :: FloatVec3, d :: FloatVec3 } deriving (Show)

originOfRay :: Ray -> FloatVec3
originOfRay (Ray o d) = o

directionOfRay :: Ray -> FloatVec3
directionOfRay (Ray o d) = d
