import Data.Char

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

pot3 n = [3^x|x<-[0..n]]

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

removerElem2 e xs = [x| x<-xs, x/=e]


ordenar [] = []
ordenar xs = min : ordenar (removerElem min xs)
            where min = menor xs

enesimo n [] = error "Lista vazia"
enesimo 0 _ = error "Nao exite p0"
enesimo 1 (x:xs) = x
enesimo n (x:xs) = enesimo (n-1) xs


repetir 0 m = [0]
repetir 1 m = [m]
repetir n m = m:repetir (n-1) m

int2dig n = toEnum (n+48)

dig2int d = (fromEnum d)-48

numString' 0 = []
numString' n = (int2dig (rem n 10)):(numString' (div n 10))

numString :: Int -> [Char]
numString n = inverso(numString' n)


stringNum' [] = []
stringNum' (x:xs) = dig2int x : stringNum' xs

stringNum'' [] = 0
stringNum'' (x:xs) = x * (10^(length xs)) + stringNum'' xs

stringNum (x:xs) = stringNum'' (stringNum' (x:xs))


bin2int' [] = 0
bin2int' (x:xs) = (ord x-48)*2^(length xs) + bin2int' xs 
bin2int xs = bin2int' xs 


int2bin' 0 = []
int2bin' n = chr ((rem n 2)+48) : int2bin' (div n 2)
int2bin n = inverso (int2bin' n)

nUltimos' _ [] = []
nUltimos' 0 _ = []
nUltimos' n (x:xs) = x: nUltimos' (n-1) xs
nUltimos n (x:xs) = inverso( nUltimos' n (inverso (x:xs)))

listaS = ["A","b","C"]
minusculas [] = []
minusculas (x:xs) = toLower x : minusculas xs