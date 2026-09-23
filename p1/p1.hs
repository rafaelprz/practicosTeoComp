module P1 where

-- 1
data Exp = Var X
    | Cons K [Exp]
    | Func X Exp
    | Apl Exp Exp
    | Case Exp [B]
    | Rec X Exp

type X = String
type K = String

type B = (K, [X], Exp)

-- 2

data Val = ConstanteV K [Val]
    | FuncionV X Exp

data Weak = ConstanteW K [Exp]
    | FuncionW X Exp


-- 3

type Sustitucion = [(X, Exp)]

efecto :: Exp -> Sustitucion -> Exp

efecto (Var x) sigma = 
    busqueda x sigma

efecto (Cons k exps) sigma = 
    Cons k (map (\e -> efecto e sigma) exps)

efecto (Func x e) sigma = 
    Func x (efecto e (bajas [x] sigma))

efecto (Apl e1 e2) sigma = 
    Apl (efecto e1 sigma) (efecto e2 sigma)

efecto (Case e bs) sigma = 
    Case (efecto e sigma) (map (\b -> efectoRamas b sigma) bs)

efecto (Rec x e) sigma = 
    Rec x (efecto e (bajas [x] sigma))

efectoRamas :: B -> Sustitucion -> B
efectoRamas (k, xs, e) sigma =
    (k, xs, (efecto e (bajas xs sigma)))

busqueda:: X -> Sustitucion -> Exp
busqueda x [] = Var x
busqueda x ((y,e):sigma)
    | x == y = e
    | otherwise = busqueda x sigma

bajas:: [X] -> Sustitucion -> Sustitucion
bajas xs sigma =
    (filter (\(x,_) -> not (x `elem` xs)) sigma)


buscarRama :: K -> [B] -> B
buscarRama k [] =
    error ("No existe una rama para " ++ k)

buscarRama k (rama@(k', _, _) : ramas)
    | k == k'   = rama
    | otherwise = buscarRama k ramas

-- Pregunta 4 --
{-

Por ejemplo, la expresión x y la tabla [x, y := y, z]:
- Simultáneamente, x se reemplaza por y. El resultado es y.
- Si primero hacés [x := y] y después [y := z], el resultado es z: la segunda sustitución modifica la y.

-}

-- Ej 5 --

evalParcial :: Exp -> Weak

evalParcial (Cons k es) = 
    ConstanteW k es

evalParcial (Func x es) = 
    FuncionW x es

evalParcial (Apl e1 e2) = 
    case evalParcial e1 of
        FuncionW x e3 -> evalParcial (efecto e3 [(x,e2)]);
        ConstanteW k es -> ConstanteW k (es ++ [e2])

evalParcial (Case e ramas) = 
    case evalParcial e of 
        ConstanteW k es -> 
            case buscarRama k ramas of
                (_, xs, cuerpo) -> evalParcial (efecto cuerpo (zip xs es))

evalParcial (Rec x e) = 
    evalParcial (efecto e [(x, Rec x e)])



-- Ej 6 --

evalFuerte :: Exp -> Val
evalFuerte e = case evalParcial e of
    ConstanteW k es -> ConstanteV k (map evalFuerte es)
    FuncionW x e' -> FuncionV x e'
    

