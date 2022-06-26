module Vec where

import Data.Word

-- TODO: refactor to use type classes and the fancy haskell stuff.
-- Yes I know its a lot of copy paste, but haskell's class system is strange.

data FloatVec3 = FloatVec3 Float Float Float deriving (Show)
data IntVec3   = IntVec3   Int   Int   Int   deriving (Show)

-- IntVec3 Code

-- -- "Constructors"

listToIntVec3 :: [Int] -> IntVec3
listToIntVec3 [a, b, c] = IntVec3 a b c

intVec3ToList :: IntVec3 -> [Int]
intVec3ToList (IntVec3 a b c) = [a, b, c]

convIntToFloatVec3 :: IntVec3 -> FloatVec3
convIntToFloatVec3 (IntVec3 a b c) = FloatVec3 (fromIntegral a) (fromIntegral b) (fromIntegral c)

-- -- Operators

intVec3Plus :: IntVec3 -> IntVec3 -> IntVec3
intVec3Plus (IntVec3 a b c) (IntVec3 d e f) = IntVec3  (a + d) (b + e) (c + f)

intVec3Minus :: IntVec3 -> IntVec3 -> IntVec3
intVec3Minus (IntVec3 a b c) (IntVec3 d e f) = IntVec3  (a - d) (b - e) (c - f)

intVec3Mul :: IntVec3 -> IntVec3 -> IntVec3
intVec3Mul (IntVec3 a b c) (IntVec3 d e f) = IntVec3  (a * d) (b * e) (c * f)

intVec3MulN :: IntVec3 -> Float -> IntVec3
intVec3MulN (IntVec3 a b c) n =
  let da = fromIntegral a
      db = fromIntegral b
      dc = fromIntegral c
      in convFloatToIntVec3 (FloatVec3 (n * da) (n * db) (n * dc))

intVec3DivN :: IntVec3 -> Float -> IntVec3
intVec3DivN v n = intVec3MulN v (1/n)

-- -- Properties

intVec3LenSq :: IntVec3 -> Float
intVec3LenSq (IntVec3 a b c) = fromIntegral $ a*a + b*b + c*c

intVec3Len :: IntVec3 -> Float
intVec3Len v = sqrt $ intVec3LenSq v

intVec3Dot :: IntVec3 -> IntVec3 -> Int
intVec3Dot (IntVec3 a b c) (IntVec3 d e f) = a*c + b*e + c*f

intVec3Cross :: IntVec3 -> IntVec3 -> IntVec3
intVec3Cross (IntVec3 a b c) (IntVec3 d e f) =
  IntVec3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

intVec3Unit :: IntVec3 -> IntVec3
intVec3Unit v = intVec3DivN v (intVec3Len v)

-- FloatVec3 Code
  
-- -- "Constructors"

listToFloatVec3 :: [Float] -> FloatVec3 
listToFloatVec3 [a, b, c] = FloatVec3 a b c

floatVec3ToList :: FloatVec3 -> [Float]
floatVec3ToList (FloatVec3 a b c) = [a, b, c]

convFloatToIntVec3 :: FloatVec3 -> IntVec3
convFloatToIntVec3 (FloatVec3 a b c) = IntVec3 (round a) (round b) (round c)

-- -- Operators

floatVec3Plus :: FloatVec3 -> FloatVec3 -> FloatVec3
floatVec3Plus (FloatVec3 a b c) (FloatVec3 d e f) = FloatVec3 (a + d) (b + e) (c + f)

floatVec3Minus :: FloatVec3 -> FloatVec3 -> FloatVec3
floatVec3Minus (FloatVec3 a b c) (FloatVec3 d e f) = FloatVec3  (a - d) (b - e) (c - f)

floatVec3Mul :: FloatVec3 -> FloatVec3 -> FloatVec3
floatVec3Mul (FloatVec3 a b c) (FloatVec3 d e f) = FloatVec3  (a * d) (b * e) (c * f)

floatVec3MulN :: FloatVec3 -> Float -> FloatVec3
floatVec3MulN (FloatVec3 a b c) n = FloatVec3 (n * a) (n * b) (n * c)

floatVec3DivN :: FloatVec3 -> Float -> FloatVec3
floatVec3DivN v n = floatVec3MulN v (1/n)

-- -- Properties

floatVec3LenSq :: FloatVec3 -> Float
floatVec3LenSq (FloatVec3 a b c) = a*a + b*b + c*c

floatVec3Len :: FloatVec3 -> Float
floatVec3Len v = sqrt $ floatVec3LenSq v

floatVec3Dot :: FloatVec3 -> FloatVec3 -> Float
floatVec3Dot (FloatVec3 a b c) (FloatVec3 d e f) = a*c + b*e + c*f

floatVec3Cross :: FloatVec3 -> FloatVec3 -> FloatVec3
floatVec3Cross (FloatVec3 a b c) (FloatVec3 d e f) =
  FloatVec3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

floatVec3Unit :: FloatVec3 -> FloatVec3
floatVec3Unit v = floatVec3DivN v (floatVec3Len v)

floatVec3ToColor :: FloatVec3 -> [Word8]
floatVec3ToColor (FloatVec3 a b c) =
  let wa = fromInteger (round (a * 255.999)) :: Word8
      wb = fromInteger (round (b * 255.999)) :: Word8
      wc = fromInteger (round (c * 255.999)) :: Word8
      in [wa, wb, wc, 255]
