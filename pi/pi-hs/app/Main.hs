
import Control.Parallel.Strategies

fact :: Integer -> Integer
fact n = fact_iter n 1

fact_iter :: Integer -> Integer -> Integer
fact_iter n acc = if n == 0 then acc else fact_iter (n - 1) (acc * n)

pow :: Integer -> Integer -> Integer
pow n m = pow_iter n m 1

pow_iter :: Integer -> Integer -> Integer -> Integer
pow_iter n m acc =
  if m == 0 then
    acc
  else
    if m `mod` 2 == 0 then
      pow_iter (n * n) (m `div` 2) (acc)
    else
      pow_iter n (m - 1) (acc * n)

iter k =
  let numerator = toRational ((fact (6*k)) * (545140134*k + 13591409))
      denominator = toRational ((fact (3*k)) * (pow (fact k) 3) * (pow (-1 * 262537412640768000) k))
  in numerator / denominator

pi_f :: Integer -> Rational
pi_f n = (426880 * (toRational (sqrt 10005))) / (sum $ (map iter [0..n] `using` parList rseq))

main :: IO ()
main = print $ show $ (fromRational (pi_f 100) :: Float)
