module Vec where

-- TODO: refactor to use type classes and the fancy haskell stuff.
-- Yes I know its a lot of copy paste, but haskell's class system is strange.

data FloatVec3 = FloatVec3 Float Float Float deriving (Show)
data IntVec3   = IntVec3   Int   Int   Int   deriving (Show)

-- IntVec3 Code

listToIntVec3 :: [Int] -> IntVec3
listToIntVec3 [a, b, c] = IntVec3 a b c

intVec3ToList :: IntVec3 -> [Int]
intVec3ToList (IntVec3 a b c) = [a, b, c]

convIntToFloatVec3 :: IntVec3 -> FloatVec3
convIntToFloatVec3 (IntVec3 a b c) = FloatVec3 (fromIntegral a) (fromIntegral b) (fromIntegral c)

addIntVec3 :: IntVec3 -> IntVec3 -> IntVec3
addIntVec3 (IntVec3 a b c) (IntVec3 d e f) = IntVec3  (a + d) (b + e) (c + f)

intVec3TimesN :: IntVec3 -> Int -> IntVec3
intVec3TimesN (IntVec3 a b c) n = IntVec3 (n * a) (n * b) (n * c)

intVec3LenSq :: IntVec3 -> Int
intVec3LenSq (IntVec3 a b c) = a*a + b*b + c*c


-- FloatVec3 Code
  
listToFloatVec3 :: [Float] -> FloatVec3 
listToFloatVec3 [a, b, c] = FloatVec3 a b c

floatVec3ToList :: FloatVec3 -> [Float]
floatVec3ToList (FloatVec3 a b c) = [a, b, c]

convFloatToIntVec3 :: FloatVec3 -> IntVec3
convFloatToIntVec3 (FloatVec3 a b c) = IntVec3 (round a) (round b) (round c)

addFloatVec3 :: FloatVec3 -> FloatVec3 -> FloatVec3
addFloatVec3 (FloatVec3 a b c) (FloatVec3 d e f) = FloatVec3 (a + d) (b + e) (c + f)

floatVec3TimesN :: FloatVec3 -> Float -> FloatVec3
floatVec3TimesN (FloatVec3 a b c) n = FloatVec3 (n * a) (n * b) (n * c)

floatVec3LenSq :: FloatVec3 -> Float
floatVec3LenSq (FloatVec3 a b c) = a*a + b*b + c*c
