module Vec where

data Vec a = Vec {x :: a, y :: a, z :: a}

type VecFloat = Vec Float
type VecInt = Vec Int
