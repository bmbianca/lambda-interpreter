module Lambda.Church where

import Lambda.AST (Term(..))

-- 1. Valori și operații booleene
cTrue :: Term
cTrue = Lambda "x" (Lambda "y" (Var "x")) 

cFalse :: Term
cFalse = Lambda "x" (Lambda "y" (Var "y"))

cAnd :: Term
cAnd = Lambda "u" (Lambda "v" (App (App (Var "u") (Var "v")) (Var "u")))

cOr :: Term
cOr = Lambda "u" (Lambda "v" (App (App (Var "u") (Var "u")) (Var "v")))

cNot :: Term 
cNot = Lambda "u" (App (App (Var "u") cFalse) cTrue)

-- 2. Numere naturale și operații
c_0 :: Term
c_0 = Lambda "f" (Lambda "x" (Var "x"))

c_1 :: Term
c_1 = Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))

c_2 :: Term
c_2 = Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (Var "x"))))

cSucc :: Term
cSucc = Lambda "n" (Lambda "f" (Lambda "x" 
        (App (App (Var "n") (Var "f")) (App (Var "f") (Var "x")))))

cPlus :: Term
cPlus = Lambda "n" (Lambda "m" (Lambda "f" (Lambda "x" 
            (App (App (Var "m") (Var "f")) 
                 (App (App (Var "n") (Var "f")) (Var "x"))))))

cMult :: Term
cMult = Lambda "n" (Lambda "m" (Lambda "f" (Lambda "x" 
            (App (App (Var "m") (App (Var "n") (Var "f"))) (Var "x")))))

-- 3. Predicate
isZero :: Term
isZero = Lambda "n" (App (App (Var "n") (Lambda "x" cFalse)) cTrue)

cMinus :: Term
cMinus = Lambda "m" (Lambda "n" (App (App (Var "n") cPred) (Var "m")))

cLeq :: Term
cLeq = Lambda "m" (Lambda "n" (App isZero (App (App cMinus (Var "m")) (Var "n"))))

cEq :: Term
cEq = Lambda "m" (Lambda "n" 
            (App (App cAnd (App (App cLeq (Var "m")) (Var "n"))) 
                 (App (App cLeq (Var "n")) (Var "m"))))

cIte :: Term
cIte = Lambda "b" (Lambda "t" (Lambda "f" (App (App (Var "b") (Var "t")) (Var "f"))))

-- 4. Perechi de numere naturale și operații
cPair :: Term
cPair = Lambda "u" (Lambda "v" (Lambda "b" 
        (App (App (Var "b") (Var "u")) (Var "v"))))

cFST :: Term
cFST = Lambda "p" (App (Var "p") cTrue )

cSND :: Term
cSND = Lambda "p" (App (Var "p") cFalse)

cAux :: Term
cAux = Lambda "p" (App 
        (App cPair (App cSND (Var "p"))) (App cSucc (App cSND (Var "p"))))

cPred :: Term
cPred = Lambda "n" (App cFST 
        (App 
            (App (Var "n") cAux) (App (App cPair c_0) c_0)))

-- 5. Liste de numere naturale

cNil :: Term
cNil = App (App cPair cTrue) cTrue

cCons :: Term
cCons = Lambda "h" (Lambda "t" 
            (App (App cPair cFalse) 
                 (App (App cPair (Var "h")) (Var "t"))))

cIsNil :: Term
cIsNil = Lambda "l" (App cFST (Var "l"))

cHead :: Term
cHead = Lambda "l" (App cFST (App cSND (Var "l")))

cTail :: Term
cTail = Lambda "l" (App cSND (App cSND (Var "l")))

-- 6. Funcții recursive (Factorial)

cY :: Term
cY = Lambda "f" 
        (App (Lambda "x" (App (Var "f") (Lambda "y" (App (App (Var "x") (Var "x")) (Var "y")))))
             (Lambda "x" (App (Var "f") (Lambda "y" (App (App (Var "x") (Var "x")) (Var "y"))))))

cFactStep :: Term
cFactStep = Lambda "f" (Lambda "n" 
                (App (App (App cIte (App isZero (Var "n"))) 
                          c_1) 
                     (App (App cMult (Var "n")) 
                          (App (Var "f") (App cPred (Var "n"))))))

cFactorial :: Term
cFactorial = App cY cFactStep

-- 7. Mediu de evaluare (Dicționar) și Macro-uri

-- | O listă care asociază string-urile pe care le introduce utilizatorul cu termenii corespunzători
churchEnv :: [(String, Term)]
churchEnv = 
    [ ("cTrue", cTrue)
    , ("cFalse", cFalse)
    , ("cAnd", cAnd)
    , ("cOr", cOr)
    , ("cNot", cNot)
    , ("c_0", c_0)
    , ("c_1", c_1)
    , ("c_2", c_2)
    , ("cSucc", cSucc)
    , ("cPlus", cPlus)
    , ("cMult", cMult)
    , ("isZero", isZero)
    , ("cMinus", cMinus)
    , ("cLeq", cLeq)
    , ("cEq", cEq)
    , ("cIte", cIte)
    , ("cPair", cPair)
    , ("cFST", cFST)
    , ("cSND", cSND)
    , ("cPred", cPred)
    , ("cNil", cNil)
    , ("cCons", cCons)
    , ("cIsNil", cIsNil)
    , ("cHead", cHead)
    , ("cTail", cTail)
    , ("cY", cY)
    , ("cFactorial", cFactorial)
    ]

-- | Parcurge termenul și înlocuiește variabilele care au același nume cu un macro din churchEnv
expandMacros :: Term -> Term
expandMacros (Var id) = case lookup id churchEnv of
                            Just termDefinition -> termDefinition -- Am găsit o encodare Church, o înlocuim
                            Nothing             -> Var id         -- E o variabilă normală, o lăsăm așa
expandMacros (App term1 term2) = App (expandMacros term1) (expandMacros term2)
expandMacros (Lambda id term)  = Lambda id (expandMacros term)