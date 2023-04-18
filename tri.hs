lista1 = [5,10,12,29]
lista2 = [1,2,3,4,5,6,7,8,9]

nprimeiros 0 xs = []
nprimeiros n [] = []
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs 

ins e [] = [e]
ins e l@(x:xs)  |e == x = l 
                |e < x = (e:l)
                |otherwise = (x:ins e xs) 

inv [] = []
inv (x:xs) = inv xs ++ [x]

pertence n [] = False
pertence n (x:xs) = if n == x then True else pertence n xs 

intercessao _ [] = []
intercessao [] _ = []
intercessao (x:xs) (y:ys) = if pertence x (y:ys) == True then x:intercessao xs (y:ys) else intercessao xs (y:ys)

