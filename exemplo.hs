-- Soma de dois números
soma :: Int -> Int -> Int
soma a b =
  a + b

-- Somar 1 + 2 + 3 + 4 + ... + n
recsum :: Int -> Int
recsum n =
  if n == 0 then
    0
  else
    n + recsum (n - 1)

-- Sequência de Fibonacci
fibonacci :: Int -> Int
fibonacci 0 =
  0
fibonacci 1 =
  1
fibonacci n =
  fibonacci (n - 1) + fibonacci (n - 2)

-- Negar um booleano
negar :: Bool -> Bool
negar True = False
negar False = True

-- Pegar o primeiro item de uma lista
primeiro :: [Int] -> Int
primeiro (x:xs) =
  x

-- Pegar a cauda de uma lista
cauda :: [Int] -> [Int]
cauda (x:xs) =
  xs

-- Pega o último item de uma lista, usando uma condicional
ultimo :: [Int] -> Int
ultimo [] = error "Não existe último item em uma lista vazia!"
ultimo (x:xs) =
  if xs == [] then
    x
  else
    ultimo xs

-- Maior elemento em uma lista utilizando um acumulador
maiorElemento :: [Int] -> Int
maiorElemento [] =
    error "Não rola"
maiorElemento (x:xs) =
    worker xs x
    where
	-- O valor de y é o maior valor visto até agora!
        worker [] y =
            y
        worker (x:xs) y =
            let maior = if x > y then
                            x
                        else
                           y in
            worker xs maior

-- Hipotenusa
hipotenusa :: Float -> Float -> Float
hipotenusa a b =
    let q1 = a * a in
    let q2 = b * b in
    let s = q1 + q2 in
    sqrt s

-- Verifica se um número existe numa lista
existeEmLista :: Eq a => a -> [a] -> Bool
existeEmLista a [] =
    False
existeEmLista a (x:xs) =
    if a == x then
        True
    else
        existeEmLista a xs
















---
