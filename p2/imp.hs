module Imp where

data Prog = Assign [X] [Exp] 
        | Local [X] Prog
        | Secuencia Prog Prog
        | Case X [B]
        | While X [B] 
        deriving Show


data Exp = Const K [Exp] | Var X deriving Show

type B = (K, [X], Prog)
type K = String
type X = String

data Val = ConstV K [Val] | Null deriving Show
type Memoria = [(X, Val)]

busqueda :: Memoria -> X -> Val
busqueda [] _ = Null
busqueda ((x1,v1):ms) x 
    | x == x1 = v1
    | otherwise = busqueda ms x

update :: Memoria -> Memoria -> Memoria
update m [] = m
update m (cambio:cambios) = update (actualizarUno m cambio) cambios

actualizarUno :: Memoria -> (X,Val) -> Memoria
actualizarUno [] (x,v) = [(x,v)]
actualizarUno ((x,v):viejas) (y,v2)
    | x == y = ((x,v2):viejas)
    | otherwise = (x,v):(actualizarUno viejas (y,v2))

alta :: [X] -> Memoria -> Memoria
alta [] m = m
alta (x:xs) m = ((x,Null) : alta xs m )

bajas :: Memoria -> [X] -> Memoria
bajas m [] = m
bajas m (x:xs) = bajas (bajaUna m x) xs

bajaUna :: Memoria -> X -> Memoria
bajaUna [] _ = []
bajaUna ((y, v):ms) x
    | x == y    = ms
    | otherwise = (y, v) : bajaUna ms x

evalExp :: Exp -> Memoria -> Val
evalExp (Var x) m = busqueda m x
evalExp (Const k es) m =
    ConstV k (map (\e -> evalExp e m) es)

evalProg :: Prog -> Memoria -> Memoria 
evalProg (Assign xs es) m = 
    update m (zip xs (map (\e -> evalExp e m) es))
evalProg (Local xs p) m = 
    case (evalProg p (alta xs m)) of
        m' -> (bajas m' xs)
evalProg (Secuencia p1 p2) m = 
    case (evalProg p1 m) of
        m' -> evalProg p2 m'
evalProg (Case x bs) m =
    let ConstV k vs = busqueda m x
        Just b = buscarRama k vs bs
    in ejecutarRama b vs m
evalProg (While x bs) m =
    let ConstV k vs = busqueda m x
    in case buscarRama k vs bs of
        Nothing -> m
        Just b -> evalProg (While x bs) (ejecutarRama b vs m)

buscarRama :: K -> [Val] -> [B] -> Maybe B
buscarRama _ _ [] = Nothing
buscarRama k vs (b@(kRama, xs, _):bs)
    | k == kRama && length xs == length vs = Just b
    | otherwise = buscarRama k vs bs

ejecutarRama :: B -> [Val] -> Memoria -> Memoria
ejecutarRama (_, xs, p) vs m =
    bajas (evalProg p (update (alta xs m) (zip xs vs))) xs

parImp :: Prog
parImp = Local ["n'"] (
    Secuencia
        (Assign ["n'", "res2"] [Var "n", Const "True" []])
        (While "n'"
            [ ("S", ["x"],
                Secuencia
                    (Assign ["n'"] [Var "x"])
                    (Case "res2" [
                        ("True", [], Assign ["res2"] [Const "False" []]),
                        ("False", [], Assign ["res2"] [Const "True" []])
                    ])
              )
            ]
        )
    )

sumaImp :: Prog
sumaImp = Local ["n'"] (
    Secuencia
        (Assign ["n'", "res"] [Var "n1", Var "n2"])
        (While "n'"
            [ ("S", ["x"], 
                Assign ["n'", "res"] [Var "x", Const "S" [Var "res"]]
            )]
        )
    )

largoImp :: Prog
largoImp = Local ["l"] (
    Secuencia 
        (Assign ["l", "res"] [Var "list", Const "O" []])
        (While "l" 
            [("Cons", ["h", "t"],
                Assign ["l", "res"] [Var "t", Const "S" [Var "res"]]
            )]
        )
    
    )

igualdadNImp :: Prog
igualdadNImp = Local ["n'", "n''"] (
    Secuencia
        (Assign ["n'", "n''", "res"]
            [Var "n1", Var "n2", Const "True" []])
        (Secuencia
            (While "n'"
                [ ("S", ["x"],
                    Case "n''"
                        [ ("S", ["y"],
                            Assign ["n'", "n''"] [Var "x", Var "y"])
                        , ("O", [],
                            Assign ["n'", "res"]
                                [Const "O" [], Const "False" []])
                        ]
                  )
                ]
            )
            (Case "n''"
                [ ("S", ["y"], Assign ["res"] [Const "False" []])
                , ("O", [], Assign ["res"] [Var "res"])
                ]
            )
        )
    )

concatImp :: Prog
concatImp = Local ["l", "rev"] (
    Secuencia
        (Assign ["l", "rev", "res"]
            [Var "l1", Const "Nil" [], Var "l2"])
        (Secuencia
            (While "l"
                [ ("Cons", ["h", "t"],
                    Assign ["l", "rev"]
                        [Var "t", Const "Cons" [Var "h", Var "rev"]]
                  )
                ]
            )
            (While "rev"
                [ ("Cons", ["h", "t"],
                    Assign ["rev", "res"]
                        [Var "t", Const "Cons" [Var "h", Var "res"]]
                  )
                ]
            )
        )
    )