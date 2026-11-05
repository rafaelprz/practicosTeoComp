-- ej 1 --

type Cinta = ([Simbolo], Simbolo, [Simbolo])

data Accion
    = Izquierda
    | Derecha
    | Escribir Simbolo
    deriving (Eq, Show)


data Estado = O | S Estado deriving(Eq, Show)

type Ramas = [(Simbolo, (Accion, Estado))]
type Codigo = [(Estado, Ramas)]

type Simbolo = String

-- ej 2 -- 

exec :: Codigo -> Cinta -> Cinta
exec cod cin =
    undefined

iteracion :: Codigo -> Estado -> Cinta -> Cinta
iteracion cod act cin@(l, simbolo, r) =
    case act of
        O -> cin
        S x -> case (lkup act simbolo cod) of 



lkup :: Estado -> Simbolo -> Codigo -> (Accion, Estado)
lkup = undefined
