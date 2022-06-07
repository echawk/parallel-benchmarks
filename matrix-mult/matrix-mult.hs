--ASSUMES ALL MATRICES ARE SQUARE!

-- TODO: Add in parallel computation

type Matrix = [[Int]]
type Vector =  [Int]
type Scalar =   Int

n_in_lst :: Int -> [a] -> a
n_in_lst n lst = last $ take n lst

col_n_matrix :: Int -> Matrix -> Vector
col_n_matrix c m = map (n_in_lst c) m

row_n_matrix :: Int -> Matrix -> Vector
row_n_matrix 1 m = head m
row_n_matrix r m = row_n_matrix (r - 1) (tail m)

row_col_mult :: Vector -> Vector -> Scalar
row_col_mult row_v col_v = sum $ map (\(a,b) -> a * b) $ zip row_v col_v

matrix_mult :: Matrix -> Matrix -> Matrix
matrix_mult m1 m2 =
    let aux_mult = (\row_v -> map (\x -> row_col_mult row_v (col_n_matrix x m2)) [1..(length m2)]) in
    map (\x -> aux_mult $ row_n_matrix x m1) [1..(length m1)]
