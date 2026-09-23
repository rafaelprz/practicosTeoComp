module P1 where


-- 0)
data Exp = Var X
    | Cons K [Exp]
    | Func X Exp
    | Apl Exp Exp
    | Case Exp [B]
    | Rec X Exp

type X = String
type K = String

data B = Rama K [X] Exp

-- 1)
data Val = ConstanteV K [Val]
    | FuncionV X Exp

data Weak = ConstanteW K [Exp]
    | FuncionW X Exp


-- 2)
data Sustitucion = [(X, Exp)]

efecto :: Exp -> Sustitucion -> Exp
efecto (Var x) sigma = busqueda x sigma
efecto (Cons k exps) sigma = Cons (k (map (\e -> efecto e sigma) exps))
efecto (Func x e) sigma = Func (x efecto(e (bajas x sigma)))
efecto (Apl e1 e2) sigma = Apl((efecto e1 sigma) (efecto e2 sigma))
efecto (Case e bs) sigma = Case((efecto e sigma) (map (\b -> efectoRamas b sigma) bs))
efecto (Rec x e) sigma = Rec (x efecto(e (bajas x sigma)))



efectoRamas :: B -> Sustitucion -> B
efectoRamas (Rama k xs e) sigma = Rama k xs (efecto e (bajas xs sigma))


busqueda:: X -> Sustitucion -> Exp
busqueda x [] = Var x
busqueda x ((y,e):sigma)
    | x == y = e
    | otherwise = busqueda x sigma

bajas:: [X] -> Sustitucion -> Sustitucion
bajas xs sigma = (filter (\(x,_) -> not (x elem xs)) sigma)