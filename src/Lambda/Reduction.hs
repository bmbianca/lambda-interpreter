module Lambda.Reduction (
    reduce1,
    reduce,
    reduceCBN,
    reduceCBV,
    reduceAO,
    strategy1,
    strategy,
    isValue
) where

import Lambda.AST

-- 1. Normal Order Strategy
reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var _) _ = Nothing
reduce1' (App (Lambda id term) term') avoid = Just (casubst id term' term avoid)
reduce1' (App term1 term2) avoid = 
    case reduce1' term1 avoid of
        Just term1' -> Just (App term1' term2)
        Nothing -> case reduce1' term2 avoid of
            Just term2' -> Just (App term1 term2')
            Nothing -> Nothing
reduce1' (Lambda id term) avoid = 
    case reduce1' term avoid of
        Just term' -> Just (Lambda id term')
        Nothing -> Nothing

reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)

reduce :: Term -> Term
reduce term = case reduce1 term of
    Nothing -> term
    Just term' -> reduce term'

-- 2. Call-by-Name (CBN)
reduceCBN' :: Term -> [Id] -> Maybe Term
reduceCBN' (Var _) _ = Nothing
reduceCBN' (App (Lambda id term) term') avoid = Just (casubst id term' term avoid) 
reduceCBN' (App term1 term2) avoid =
    case reduceCBN' term1 avoid of
        Just term1' -> Just (App term1' term2) 
        Nothing -> Nothing
reduceCBN' (Lambda _ _) _ = Nothing 

reduceCBN :: Term -> Maybe Term
reduceCBN t = reduceCBN' t (vars t)

-- 3. Call-by-Value (CBV)
isValue :: Term -> Bool
isValue (Lambda _ _) = True
isValue _ = False

reduceCBV' :: Term -> [Id] -> Maybe Term
reduceCBV' (Var _) _ = Nothing
reduceCBV' (Lambda _ _) _ = Nothing
reduceCBV' (App (Lambda id term) term') avoid
    | isValue term' = Just (casubst id term' term avoid)
    | otherwise = case reduceCBV' term' avoid of
                    Just term'' -> Just (App (Lambda id term) term'')
                    Nothing -> Nothing
reduceCBV' (App term1 term2) avoid = 
    case reduceCBV' term1 avoid of
        Just term1' -> Just (App term1' term2)
        Nothing -> if isValue term1 then 
                    case reduceCBV' term2 avoid of
                        Just term2' -> Just (App term1 term2')
                        Nothing -> Nothing
                    else Nothing

reduceCBV :: Term -> Maybe Term
reduceCBV t = reduceCBV' t (vars t)

-- 4. Applicative Order
reduceAO' :: Term -> [Id] -> Maybe Term
reduceAO' (Var _) _ = Nothing
reduceAO' (Lambda id term) avoid =
    case reduceAO' term avoid of
        Just term' -> Just (Lambda id term') 
        Nothing -> Nothing
reduceAO' (App term1 term2) avoid =
    case reduceAO' term1 avoid of
        Just term1' -> Just (App term1' term2) 
        Nothing -> case reduceAO' term2 avoid of
            Just term2' -> Just (App term1 term2')
            Nothing -> case term1 of
                Lambda id term' -> Just (casubst id term2 term' avoid)
                _ -> Nothing

reduceAO :: Term -> Maybe Term
reduceAO t = reduceAO' t (vars t)

-- 5. Full Beta-Reduction
strategy1' :: Term -> [Id] -> [Term]
strategy1' (Var _) _ = []
strategy1' (App (Lambda id term) term') avoid = [casubst id term' term avoid] ++
    let allSteps = strategy1' term avoid in
    let allSteps' = strategy1' term' avoid in
    [ App (Lambda id successorTerm) term' | successorTerm <- allSteps ] ++
    [ App (Lambda id term) successorTerm' | successorTerm' <- allSteps']
strategy1' (App term1 term2) avoid =
    let all1 = strategy1' term1 avoid in
    let all2 = strategy1' term2 avoid in
    [ App sterm1 term2 | sterm1 <- all1 ] ++
    [ App term1 sterm2 | sterm2 <- all2 ]
strategy1' (Lambda id term) avoid =
    let allSteps = strategy1' term avoid in
    [ Lambda id sterm | sterm <- allSteps ]

strategy1 :: Term -> [Term]
strategy1 term = strategy1' term (vars term)

strategy :: Term -> [Term]
strategy term = let allSteps = strategy1 term in case allSteps of
    [] -> [term]
    _ -> concatMap strategy allSteps