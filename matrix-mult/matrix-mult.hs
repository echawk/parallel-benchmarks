

type Matrix = [[Integer]]
type Vector =  [Integer]

--mult_matr :: Matrix -> Matrix -> Matrix
--mult_matr A B =

n_in_lst :: Int -> [a] -> a
n_in_lst n lst = head $ reverse $ take n lst 

col_n_matrix :: Int -> Matrix -> Vector
col_n_matrix c m = map (n_in_lst c) m

row_n_matrix :: Integer -> Matrix -> Vector
row_n_matrix 1 m = head m
row_n_matrix r m = row_n_matrix (r - 1) (tail m)

                   
ex = [[1, 2, 3],
      [4, 5, 6],
      [7, 8, 9]]

ex2 = [[1, 2, 3],
       [0, 0, 0],
       [7, 8, 9]]

--foldl (+) 0 $ map (\(a,b) -> a * b) $ zip (row_n_matrix 1 ex) (col_n_matrix 1 ex2)

