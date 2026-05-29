module Lambda.AST (
    Id,
    Term(..),
    subst,
    remove,
    free,
    vars,
    fresh,
    casubst
) where

type Id = String

-- Reprezentarea unui lambda-termen
data Term = Var Id
          | App Term Term
          | Lambda Id Term 
          deriving (Show, Eq)

-- Substituția simplă (folosită ca pas intermediar)
subst :: Id -> Term -> Term -> Term
subst id term (Var id') 
    | id == id' = term
    | True      = Var id'
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term') 
    | id == id' = Lambda id' term'
    | True      = Lambda id' (subst id term term')

-- Eliminarea elementelor dintr-o listă
remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) 
    | id == hd  = remove id tl
    | True      = hd : remove id tl

-- Calcularea variabilelor libere
free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)

-- Calcularea tuturor variabilelor (libere sau legate)
vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) = id : vars term

-- Generarea unui identificator proaspăt (fresh) pentru a evita capturarea
fresh' :: [Id] -> Int -> Id
fresh' ids index = if ("n" ++ show index) `elem` ids 
                   then fresh' ids (index + 1) 
                   else "n" ++ show index

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

-- Capture-avoiding substitution
casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') avoid
    | id == id' = term
    | True      = Var id'
casubst id term (App term1 term2) avoid = 
    App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid
    | id == id' = Lambda id' term'
    | id' `elem` free term = 
        let id'' = fresh avoid in
        Lambda id'' (casubst id term (subst id' (Var id'') term') (id'' : avoid))
    | True = Lambda id' (casubst id term term' avoid)