import Test.Hspec
import Lambda.AST
import Lambda.Reduction
import Lambda.Parser (parseTerm, pTerm)
import Lambda.Church (expandMacros)
import Text.Megaparsec (parseMaybe)

-- | Helper pentru a parsa rapid un string in teste
parseT :: String -> Term
parseT input = case parseMaybe pTerm input of
    Just t  -> t
    Nothing -> error $ "Eroare de parsare la test: " ++ input

-- | Helper pentru evaluare completa (Normal Order) folosind macro-urile Church
evalNO :: String -> Term
evalNO input = reduce (expandMacros (parseT input))

main :: IO ()
main = hspec $ do
  describe "1. Valori si operatii booleene" $ do
    it "cAnd cTrue cFalse -> cFalse" $ do
      evalNO "cAnd cTrue cFalse" `shouldBe` evalNO "cFalse"
    it "cOr cFalse cTrue -> cTrue" $ do
      evalNO "cOr cFalse cTrue" `shouldBe` evalNO "cTrue"
    it "cNot cTrue -> cFalse" $ do
      evalNO "cNot cTrue" `shouldBe` evalNO "cFalse"

  describe "2. Numere naturale si operatii" $ do
    it "cSucc c_1 -> c_2" $ do
      evalNO "cSucc c_1" `shouldBe` evalNO "c_2"
    it "cPlus c_1 c_2 -> c_3 (testat prin echivalenta cu succ c_2)" $ do
      evalNO "cPlus c_1 c_2" `shouldBe` evalNO "cSucc c_2"
    it "cMult c_2 c_2 -> c_4 (testat prin echivalenta cu c_2 + c_2)" $ do
      evalNO "cMult c_2 c_2" `shouldBe` evalNO "cPlus c_2 c_2"

  describe "3. Predicate" $ do
    it "isZero c_0 -> cTrue" $ do
      evalNO "isZero c_0" `shouldBe` evalNO "cTrue"
    it "cEq c_1 c_1 -> cTrue" $ do
      evalNO "cEq c_1 c_1" `shouldBe` evalNO "cTrue"
    it "cIte (cEq c_1 c_1) c_1 c_2 -> c_1" $ do
      evalNO "cIte (cEq c_1 c_1) c_1 c_2" `shouldBe` evalNO "c_1"

  describe "4. Perechi de numere naturale" $ do
    it "cFST (cPair c_2 c_0) -> c_2" $ do
      evalNO "cFST (cPair c_2 c_0)" `shouldBe` evalNO "c_2"
    it "cSND (cPair c_2 c_0) -> c_0" $ do
      evalNO "cSND (cPair c_2 c_0)" `shouldBe` evalNO "c_0"

  describe "5. Liste de numere naturale" $ do
    it "cIsNil cNil -> cTrue" $ do
      evalNO "cIsNil cNil" `shouldBe` evalNO "cTrue"
    it "cIsNil (cTail (cCons c_1 cNil)) -> cTrue" $ do
      evalNO "cIsNil (cTail (cCons c_1 cNil))" `shouldBe` evalNO "cTrue"
    it "cHead (cCons c_2 cNil) -> c_2" $ do
      evalNO "cHead (cCons c_2 cNil)" `shouldBe` evalNO "c_2"

  describe "6. Functii recursive" $ do
    it "cFactorial c_1 -> c_1" $ do
      evalNO "cFactorial c_1" `shouldBe` evalNO "c_1"

  describe "Diferente intre Strategiile de Reducere" $ do
    it "1. Normal Order (NO) - Evalueaza exteriorul si scapa de bucla infinita" $ do
      let term = parseT "(\\x.y) ((\\z.z z) (\\w.w w))"
      reduce term `shouldBe` Var "y"

    it "2. Call-by-Name (CBN) - Nu evalueaza in interiorul unei abstractii Lambda" $ do
      let term = parseT "\\a. ((\\x.x) a)"
      reduceCBN term `shouldBe` Nothing

    it "3. Call-by-Value (CBV) - Forteaza argumentul mai intai (1 pas din bucla Omega)" $ do
      let term = parseT "(\\x.y) ((\\z.z z) (\\w.w w))"
      let expected = parseT "(\\x.y) ((\\w.w w) (\\w.w w))"
      reduceCBV term `shouldBe` Just expected

    it "4. Applicative Order (AO) - Evalueaza interiorul chiar si sub Lambda" $ do
      let term = parseT "\\a. ((\\x.x) a)"
      let expected = parseT "\\a.a"
      reduceAO term `shouldBe` Just expected

    it "5. Full Beta-Reduction (FULL) - Returneaza urmatorul pas posibil din graf" $ do
      let term = parseT "(\\x.x x) (\\y.y)"
      let expected = parseT "(\\y.y) (\\y.y)"
      strategy1 term `shouldBe` [expected]

  describe "7. Cazuri Limita (Edge Cases) Matematice" $ do
    it "cPred c_0 -> c_0 (predecesorul lui 0 in aritmetica Church ramane 0)" $ do
      evalNO "cPred c_0" `shouldBe` evalNO "c_0"
    
    it "cMinus c_1 c_2 -> c_0 (scaderea sub zero se opreste la 0)" $ do
      evalNO "cMinus c_1 c_2" `shouldBe` evalNO "c_0"

  describe "8. Erori de Parsare (Negative Testing)" $ do
    it "Intoarce Nothing pentru o expresie cu paranteze neinchise" $ do
      parseMaybe pTerm "(\\x.x" `shouldBe` Nothing
    
    it "Intoarce Nothing pentru o sintaxa lambda incorecta (lipseste punctul)" $ do
      parseMaybe pTerm "\\x x" `shouldBe` Nothing