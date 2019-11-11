import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type
          deriving Show

type Name = String

type Unifier = [(Name, Type)]

int_prim :: Parser Type
int_prim = do
  char 'i'
  char 'n'
  char 't'
  return TypeInt

variable :: Parser Type
variable = do
  name <- many1 lower
  return (TypeVar name)

function :: Parser Type
function = do
  left <- atomic
  char '-'
  char '>'
  right <- parse_type
  return (TypeArrow left right)

atomic :: Parser Type
atomic =
  try int_prim <|> variable <|> parentheses

parentheses :: Parser Type
parentheses = do
  char '('
  t <- parse_type
  char ')'
  return t

parse_type :: Parser Type
parse_type =
  try function <|> try int_prim <|> variable

main = do
  putStrLn "Digite o tipo A: "
  a <- getLine
  putStrLn "Digite o tipo B: "
  b <- getLine
  --
  case (parse parse_type "<stdin>" a, parse parse_type "<stdin>" b) of
    (Right ta, Right tb) ->
      let mgu = unify ta tb in
      putStrLn ("Unificação A ~ B = " ++ show mgu)
    _ -> do
      putStrLn "Erro de parsing! Tente novamente!"

unify :: Type -> Type -> Maybe Unifier

-- Regra (REFL)
unify (TypeVar a) (TypeVar b) | a == b =
  Just []

-- Regra (LEFT)
unify (TypeVar a) t | not (occursCheck a t) =
  Just [(a, t)]

-- Regra (RIGHT)
unify t (TypeVar a) | not (occursCheck a t) =
  Just [(a, t)]

-- Regra (INT)
unify (TypeInt) (TypeInt) =
  Just []

-- Regra (ARROW)
unify (TypeArrow t1 r1) (TypeArrow t2 r2) = do
  theta1 <- unify t1 t2
  theta2 <- unify (subst theta1 r1) (subst theta1 r2)
  --
  Just (compose theta2 theta1)

-- Caso geral (não dá pra unificar)
unify a b =
  Nothing


subst :: Unifier -> Type -> Type
subst u (TypeInt) =
  TypeInt
subst u (TypeVar name) =
  -- lookup :: Eq a => a -> [(a, b)] -> Maybe b
  case lookup name u of
    -- Se existia na lista, retorna a substituição!
    Just t -> t
    -- Senão, retorna o original!
    Nothing -> TypeVar name
subst u (TypeArrow t1 t2) =
  -- Posso substituir variáveis dentro de uma seta,
  -- mas continua sendo uma seta!
  TypeArrow (subst u t1) (subst u t2)

occursCheck :: Name -> Type -> Bool
occursCheck name (TypeInt) =
  False
occursCheck name (TypeVar x) =
  name == x
occursCheck name (TypeArrow t1 t2) =
  occursCheck name t1 || occursCheck name t2

concatenar :: [a] -> [a] -> [a]
concatenar = (++)

mapear :: (a -> b) -> [a] -> [b]
mapear = fmap

compose :: Unifier -> Unifier -> Unifier
compose xs ys =
  --            Para cada item y na lista,
  --              aplique subst_in_unifier xs y
  concatenar xs (mapear (subst_in_unifier xs) ys)

subst_in_unifier :: Unifier -> (Name, Type) -> (Name, Type)
subst_in_unifier u (name, t) =
  -- Substituição dentro de um unificador
  (name, subst u t)
