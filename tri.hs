lista1 = [5,10,12,29]
lista2 = [1,2,3,4,5,6,7,8,9]
lista3 = [4,3,89,3,4,56]

nprimeiros 0 xs = []
nprimeiros n [] = []
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs 

ins e [] = [e]
ins e l@(x:xs)  |e == x = l 
                |e < x = (e:l)
                |otherwise = (x:ins e xs) 

inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

pertence n [] = False
pertence n (x:xs) = if n == x then True else pertence n xs 

intercessao _ [] = []
intercessao [] _ = []
intercessao (x:xs) (y:ys) = if pertence x (y:ys) == True then x:intercessao xs (y:ys) else intercessao xs (y:ys)

soma2 xs [] = xs
soma2 [] ys = ys
soma2 (x:xs) (y:ys) = (x + y):soma2 xs ys

pot2' 0 = []
pot2' n = (2^n) : pot2' (n-1)

pot2 n = inverso (pot2' n)

intercalacao [] (y:ys) = ys
intercalacao (x:xs) [] = xs
intercalacao (x:xs) (y:ys) |x > y = y:intercalacao (x:xs) ys
                           |y > x = x:intercalacao xs (y:ys)
                           |otherwise = x:y:intercalacao xs ys

menor [x] = x
menor (x:xs) = if x < m then x else m
    where 
    m = menor xs


removerElem n [] = []
removerElem n (x:xs) = if n == x then xs else x: removerElem n xs




