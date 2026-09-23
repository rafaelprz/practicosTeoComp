module P0 where
import Text.Parsec (parseTest)
import Distribution.SPDX.LicenseId (LicenseId(ZPL_1_1))

--- 1) ---

data Exp = Var X 
        | Empty 
        | Unit Z 
        | Pert Z Exp 
        | Union Exp Exp 
        | Inter Exp Exp 
        | Diff Exp Exp 
        | Incl Exp Exp
        | Power Exp
        | Assign X Exp
        | Iguales Exp Exp
        | Largo Exp
    deriving Show

type X = String
type Z = Int

--- 2) ---

data Val = Num Z | Conj[Z] | Bool Bool | Potencia[[Z]] deriving Show

--- 3) ---

type Memoria a b = [(a, b)]

create :: Memoria a b
create = []

update :: Eq a => a -> b -> Memoria a b -> Memoria a b
update k v [] = [(k, v)]
update k v ((k1, v1): kvs)
    | k == k1 = (k, v):kvs
    | otherwise = (k1, v1):(update k v kvs)

lkup :: Eq a => a -> Memoria a b -> b
lkup k [] = error "not found"
lkup k ((k1, v1):kvs)
    | k == k1 = v1
    | otherwise = lkup k kvs

-- del :: Eq a => a -> Tabla a b -> Tabla a b
-- del k t = filter ( \(x,y) -> x /= k ) t

--- 4) ---
belongs :: Z -> [Z] -> Bool
belongs z [] = False
belongs z (x:xs)
    | z == x = True
    | otherwise = belongs z xs

union :: [Z] -> [Z] -> [Z]
union [] l = l
union l [] = l
union (x1:xs1) l
    | belongs x1 l = union xs1 l 
    | otherwise = x1:union xs1 l

intersection :: [Z] -> [Z] -> [Z]
intersection [] l = []
intersection l [] = []
intersection (x1:xs1) l
    | belongs x1 l = x1:intersection xs1 l
    | otherwise = intersection xs1 l

difference :: [Z] -> [Z] -> [Z]
difference [] l = []
difference l [] = l
difference (x1:xs1) l
    | belongs x1 l = difference xs1 l
    | otherwise = x1:difference xs1 l

included :: [Z] -> [Z] -> Bool
included [] l = True
included l [] = False
included c1 c2
    | (difference c1 c2) == [] = True
    | otherwise = False

powerSet :: [Z] -> [[Z]]
powerSet [] = [[]]
powerSet(x:xs) =
    let partes = powerSet xs
    in partes ++ map (x:) partes

largo :: [Z] -> Z
largo [] = 0
largo (x:xs) 
    | belongs x xs = largo xs
    | otherwise = 1 + largo xs

equal :: [Z] -> [Z] -> Bool
equal xs ys = included xs ys && included ys xs

eval :: Memoria X Val -> Exp -> (Memoria X Val, Val)
eval m (Var x) = (m, lkup x m)
eval m Empty = (m, Conj [])
eval m (Unit z) = (m, Conj [z])

eval m (Largo e) =
    case eval m e of 
        (m', Conj c) -> (m', Num (largo c))

eval m (Iguales e1 e2) =
    let (m', Conj c1) = eval m e1
        (m'', Conj c2) = eval m' e2
    in ( m'', Bool (equal c1 c2))

eval m (Pert z e) =
    case eval m e of 
        (m', Conj c) -> (m', Bool (belongs z c))

eval m (Power e) =
    case eval m e of 
        (m', Conj c) -> (m', Potencia (powerSet c))

eval m (Union e1 e2) =
    let (m', Conj c1) = eval m e1
        (m'', Conj c2) = eval m' e2
    in (m'', Conj (union c1 c2))

eval m (Inter e1 e2) =
    let (m', Conj c1) = eval m e1
        (m'', Conj c2) = eval m' e2
    in (m'', Conj (intersection c1 c2))

eval m (Diff e1 e2) =
    let (m', Conj c1) = eval m e1
        (m'', Conj c2) = eval m' e2
    in (m'', Conj (difference c1 c2))

eval m (Incl e1 e2) =
    let (m', Conj c1) = eval m e1
        (m'', Conj c2) = eval m' e2
    in (m'', Bool (included c1 c2))

eval m (Assign x e) =
    case eval m e of
        (m', v) -> (update x v m', v)


-- Parte 5 --
conj1 :: Exp
conj1 = Union (Unit 1) (Union (Unit 2) (Unit 3))

conj2 :: Exp
conj2 = Union (Unit 2) (Union (Unit 3) (Unit 4))

conj3 :: Exp 
conj3 = Union conj1 conj2

conj4 :: Exp 
conj4 = Inter conj1 conj2

pert1 :: Exp
pert1 = Pert 2 conj1

pert2 :: Exp
pert2 = Pert 3 conj4

incl1 :: Exp
incl1 = Incl conj1 conj2

incl2 :: Exp
incl2 = Incl conj4 conj2

incl3 :: Exp
incl3 = Incl conj1 conj3

ass1 :: Exp
ass1 = Assign "w" conj1

ass2 :: Exp
ass2 = Assign "x" conj4

ass3 :: Exp
ass3 = Assign "y" pert2

ass4 :: Exp
ass4 = Assign "z" incl2
