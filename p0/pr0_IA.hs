module PR0 where

type X = String
type Z = Integer

-- 1.1
data Exp = Var X
        | Empty
        | Unit Z 
        | Pert Z Exp
        | Union Exp Exp
        | Inter Exp Exp
        | Diff Exp Exp
        | Incl Exp Exp
        | Assig X Exp
    deriving Show

-- 2.1
data Val = Boolean Bool | Conj [Z]

-- 3.1
type Memoria a b = [(a, b)]
-- 3.2
lkup :: Eq a => a -> Memoria a b -> b
lkup k [] -> error "not found"
lkup k ((k1, v1):kvs) = 
    | k == k1 = v1
    | otherwise = lkup k kvs
-- 3.3
update :: Eq a => a -> b -> Memoria a b -> Memoria a b
update k v [] = [(k, v)]
update k v ((k1, v1): kvs)
    | k == k1 = (k, v):kvs
    | otherwise = (k1, v1):(update k v kvs) 

-- 4.1
belongs :: Int -> [Int] -> Bool
belongs z [] = False
belongs z (x:xs)
    | z == x = True
    | otherwise = belongs z xs

union :: [Int] -> [Int] -> [Int]
union [] l = l
union l [] = l
union (x1:xs1) l
    | belongs x1 l = union xs1 l 
    | otherwise = x1:union xs1 l

intersection :: [Int] -> [Int] -> [Int]
intersection [] l = []
intersection l [] = []
intersection (x1:xs1) l
    | belongs x1 l = x1:intersection xs1 l
    | otherwise = intersection xs1 l

difference :: [Int] -> [Int] -> [Int]
difference [] l = []
difference l [] = l
difference (x1:xs1) l
    | belongs x1 l = difference xs1 l
    | otherwise = x1:difference xs1 l

included :: [Int] -> [Int] -> Bool
included [] l = True
included l [] = False
included c1 c2
    | (difference c1 c2) == [] = True
    | otherwise = False

-- 4.2
eval :: Memoria X Val -> Exp -> (Memoria X Val, Val)
eval m (Var x) = (m, lkup x m)
eval m Empty = (m, Conj[])
eval m (Unit z) = (m, Conj[z])
eval m (Pert z e) =
    case eval m e of
        (m', Conj c) -> (m', Boolean (belongs z c))
        _ -> error "Pert esperaba una expresion de conjunto"
eval m (Union e1 e2) = 
    case eval m e1 of 
        (m', Conj c1) -> case eval m' e2 of 
            (m'', Conj c2) -> (m'', Conj (union c1 c2))
            _ -> error ""
        _ -> error ""
eval m (Inter e1 e2) = 
    case eval m e of 
        (m', Conj c1) -> case eval m' e2 of 
            (m'', Conj c2) -> (m'', Conj (intersection c1 c2))
            _ -> error ""
        _ -> error ""
eval m (Diff e1 e2) = 
    case eval m e of 
        (m', Conj c1) -> case eval m' e2 of 
            (m'', Conj c2) -> (m'', Conj (difference c1 c2))
            _ -> error ""
        _ -> error ""
eval m (Incl e1 e2) =  =
    case eval m e of 
        (m', Conj c1) -> case eval m' e2 of 
            (m'', Conj c2) -> (m'', Boolean (included c1 c2))
            _ -> error ""
        _ -> error ""
eval m (Assign x e) =
  let (m', v) = eval m e
   in (upd m' (x, v), v)


--- 5) ---
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
ass1 = Assing "w" conj1

ass2 :: Exp
ass2 = Assing "x" conj4

ass3 :: Exp
ass3 = Assing "y" pert2

ass4 :: Exp
ass4 = Assing "z" incl2