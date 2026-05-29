# Interpretor Lambda-Calcul

Acest proiect implementează un interpretor de bază pentru lambda-calcul, dezvoltat în Haskell folosind utilitarul `stack`. Proiectul a fost creat ca lucrare de final de semestru pentru cursul de Programare Funcțională, asamblând și modularizând conceptele studiate pe parcursul laboratoarelor.

## Funcționalități Implementate

Nucleul aplicației este împărțit în module clare, fiecare acoperind o funcționalitate esențială a interpretorului:

* **Arborele de Sintaxă Abstractă (AST):** Reprezentarea internă a termenilor (variabile, aplicații, abstractizări lambda), incluzând logica pentru evaluarea variabilelor libere și substituția care evită capturarea (*capture-avoiding substitution*).
* **Parser:** Un parser robust construit cu librăria `Megaparsec` care transformă șiruri de caractere (ex: `(\x.x) y`) în termeni AST evaluabili. Parserul acceptă identificatori alfanumerici complecși, inclusiv caracterul `_`.
* **Strategii de Reducere:** Proiectul suportă mai multe strategii de evaluare la alegere:
  * `no` - Normal Order (implicită)
  * `cbn` - Call-by-Name
  * `cbv` - Call-by-Value
  * `ao` - Applicative Order
  * `full` - Full Beta-Reduction
* **Encodări Church & Extensie Macro:** Un modul dedicat (`Lambda.Church`) ce conține definițiile standard pentru lucrul cu valori booleene, numere naturale, predicate și liste. Acestea sunt conectate global în interpretor; aplicația expandează automat aceste macro-uri din terminal înainte de începerea reducerii.
* **Interfață Linie de Comandă (CLI):** O interfață de utilizare interactivă dezvoltată cu `optparse-applicative`, permițând controlul facil al numărului de pași și al strategiei.

##  Structura Proiectului

```text
lambda-interpreter/
├── app/
│   └── Main.hs                 -- Punctul de intrare (CLI și parsare argumente)
├── src/
│   ├── Lambda/
│   │   ├── AST.hs              -- Tipul Term și operațiile de bază
│   │   ├── Parser.hs           -- Analizorul sintactic extins (Megaparsec)
│   │   ├── Reduction.hs        -- Logica de aplicare a celor 5 strategii de reducere
│   │   └── Church.hs           -- Biblioteca standard Church și logica de macro-expansiune
├── test/
│   └── Spec.hs                 -- Suita extinsă de 24 de teste automate (Hspec)
├── package.yaml                -- Configurația proiectului și a dependențelor
└── README.md                   -- Documentația curentă
```

## Compilare și Rulare

Pentru a compila și rula acest proiect, asigurați-vă că aveți instalat utilitarul `stack`.

**1. Compilarea proiectului:**
```bash
stack build
```

**2. Rularea executabilului:**
Când transmiteți argumente aplicației prin `stack`, utilizați `--` pentru a separa opțiunile `stack` de opțiunile interpretorului.

**Afișarea meniului de ajutor:**
```bash
stack run -- --help
```

### Argumente Suportate
* `-s, --strategy`: Alege strategia de reducere (`no`, `cbn`, `cbv`, `ao`, `full`). Implicit este `no`.
* `-n, --steps`: Setează numărul maxim de pași de reducere pentru a preveni buclele infinite (ex: combinatorul Omega).
* `TERM`: Lambda-termenul sau expresia Church ce urmează a fi evaluată, pusă între ghilimele.

## Exemple de Utilizare

**1. Evaluare implicită (Normal Order) până la forma normală:**
```bash
stack run -- "(\x.x x) (\y.y)"
```

**2. Evaluare folosind Call-by-Name (cu număr de pași limitat la 3):**
```bash
stack run -- --strategy cbn --steps 3 "(\x.\y.x) z w"
```

**3. Evaluare folosind Call-by-Value:**
```bash
stack run -- --strategy cbv "(\x1.\x2.x2) ((\x.x) (\y.y))"
```

**4. Oprirea unei bucle infinite limitând pașii (Siguranță):**
```bash
stack run -- --steps 5 "(\x.x x) (\x.x x)"
```

**5. Integrarea Encodărilor Church (Macro-uri directe din CLI):**
```bash
stack run -- "cPlus c_1 c_1"
```

**6. Evaluarea predicatelor matematice:**
```bash
stack run -- "isZero c_0"
```

##  Testare Automată

Proiectul include o suită completă de **24 de teste unitare** scrise cu framework-ul `Hspec`, garantând o acoperire robustă și eliminarea regresivității în cod. Testele automate sunt împărțite riguros pe scenarii:
1. **Analiză Sintactică corectă:** Validarea parserului pe identificatori și expresii lambda.
2. **Evaluări de Bază:** Substituția corectă și evitarea capturării variabilelor libere.
3. **Corectitudinea Encodărilor Church:** Testarea automată a operațiilor booleene, operațiilor aritmetice, perechilor și listelor.
4. **Diferențe Teoretice de Strategii:** Teste dedicate care atestă divergența/convergența strategiilor (NO, CBN, CBV, AO, FULL) pe combinatorul Omega și abstracții.
5. **Cazuri Limită Matematice (Edge Cases):** Verificarea comportamentului la graniță (ex: Predecesorul lui zero `cPred c_0` rămâne zero, iar scăderile sub zero `cMinus c_1 c_2` se opresc la zero).
6. **Erori de Parsare (Negative Testing):** Asigurarea faptului că parserul respinge corect string-urile invalide (ex: paranteze neînchise sau lipsa punctului după legarea variabilei).

Pentru a rula întreaga suită de teste:
```bash
stack test
```
