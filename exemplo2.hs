-- Operador de bind:
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

main :: IO ()
main = do                         -- Quando usamos a do-notation,
    putStrLn "Hello, world!"      -- o Haskell irá inserir operadores
    -- putStrLn "Diga seu nome:"  -- de bind para nós entre cada uma
    -- nome <- getLine            -- das linhas, dando a impressão
    -- putStrLn ("Olá, " ++ nome) -- de um código "imperativo"
    print teste2
    subprograma

teste1 :: [Int]
teste1 = [1, 2, 3] >>=
           (\x -> [4, 5, 6] >>=
             (\y -> return (x + y)))

teste2 :: [Int]
teste2 = do
    x <- [1, 2, 3] -- Executa o código abaixo para
    y <- [4, 5, 6] -- cada um dos elementos da lista
    return (x + y) -- Define o valor de retorno

--
subprograma :: IO ()
subprograma = do
    -- Cria uma mônada sem executar nada
    numeros <- lerNumeros
    -- numeros :: [Int]
    print (converteParaArvore numeros)

--
lerNumeros :: IO [Int]
lerNumeros = do
    texto <- getLine
    let numero = read texto -- Dentro da do-notation, não há "in"!
    if numero == 0          -- Além disso, preciso indentar o "then"
    then do
          return []
    else do
          cauda <- lerNumeros
          return (numero : cauda)

--
data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            -- Pede para o Haskell uma forma de converter para String
            deriving Show

--
converteParaArvore :: [Int] -> Tree Int
converteParaArvore (x:[]) =
    Leaf x
converteParaArvore (x:xs) =
    Node (Leaf x) (converteParaArvore xs)
