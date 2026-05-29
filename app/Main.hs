module Main where

import Options.Applicative
import Lambda.AST (Term)
import Lambda.Parser (parseTerm)
import Lambda.Reduction (reduce1, reduceCBN, reduceCBV, reduceAO, strategy1)
import Text.Megaparsec.Error (errorBundlePretty)
import Lambda.Church (expandMacros)

-- | Definim strategiile suportate
data Strategy = NO | CBN | CBV | AO | FULL deriving (Read, Show, Eq)

-- | Structura care va ține argumentele primite din linia de comandă
data Options = Options
  { optStrategy :: Strategy
  , optSteps    :: Maybe Int
  , optTerm     :: String
  }

-- | Funcție de parsare pentru opțiunea de strategie
strategyReader :: ReadM Strategy
strategyReader = eitherReader $ \s -> case s of
    "no"   -> Right NO
    "cbn"  -> Right CBN
    "cbv"  -> Right CBV
    "ao"   -> Right AO
    "full" -> Right FULL
    _      -> Left "Strategie invalida. Alege din: no, cbn, cbv, ao, full"

-- | Parserul principal pentru argumentele CLI
optionsParser :: Parser Options
optionsParser = Options
  <$> option strategyReader
      ( long "strategy"
     <> short 's'
     <> value NO
     <> help "Strategia de reducere: no (Normal Order), cbn (Call-by-Name), cbv (Call-by-Value), ao (Applicative Order), full (Full Beta)" )
  <*> optional (option auto
      ( long "steps"
     <> short 'n'
     <> metavar "STEPS"
     <> help "Numarul maxim de pasi de evaluare (optional)" ))
  <*> strArgument
      ( metavar "TERM"
     <> help "Lambda-termenul de evaluat (ex: \"(\\x.x) y\")" )

-- | Execută pașii de reducere iterativ pentru strategiile clasice
runSteps :: Maybe Int -> (Term -> Maybe Term) -> Term -> IO ()
runSteps (Just 0) _ t = putStrLn $ "Limita de pasi atinsa. Termen curent:\n" ++ show t
runSteps steps f t = do
    print t
    case f t of
        Nothing -> putStrLn "-> S-a ajuns la forma normala."
        Just t' -> runSteps (fmap (\x -> x - 1) steps) f t'

-- | Execută pașii pentru Full Beta-Reduction (afișând prima cale găsită din graf)
runFullSteps :: Maybe Int -> Term -> IO ()
runFullSteps (Just 0) t = putStrLn $ "Limita de pasi atinsa. Termen curent:\n" ++ show t
runFullSteps steps t = do
    print t
    let nexts = strategy1 t
    case nexts of
        [] -> putStrLn "-> S-a ajuns la forma normala."
        (t':_) -> do
            putStrLn "   (Aplicam Full Beta, urmand prima reducere posibila)"
            runFullSteps (fmap (\x -> x - 1) steps) t'

-- | Funcția principală care asamblează CLI-ul
main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Evalueaza un lambda-termen pas cu pas."
     <> header "lambda-interpreter - Proiect Programare Functionala" )

-- | Logica de potrivire a datelor parsate cu acțiunile interpretorului
run :: Options -> IO ()
run (Options strat steps input) = do
    case parseTerm input of
        Left err -> putStrLn $ "Eroare de parsare:\n" ++ errorBundlePretty err
        Right parsedTerm -> do
            let term = expandMacros parsedTerm
            putStrLn $ "== Evaluare " ++ show strat ++ " =="
            case strat of
                NO   -> runSteps steps reduce1 term
                CBN  -> runSteps steps reduceCBN term
                CBV  -> runSteps steps reduceCBV term
                AO   -> runSteps steps reduceAO term
                FULL -> runFullSteps steps term