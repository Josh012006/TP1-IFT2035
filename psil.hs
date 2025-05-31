-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> not (isAscii c)
                                          || c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String
type Constructor = Var
type Lpat = Maybe (Constructor, [Var])

data Lexp = Lnum Int                    -- Constante entière.
          | Lvar Var                    -- Référence à une variable.
          | Labs Var Lexp               -- Fonction anonyme prenant un argument.
          | Lapply Lexp Lexp            -- Appel de fonction, avec un argument.
          | Lnew Constructor [Lexp]
          | Lfilter Lexp [(Lpat, Lexp)] -- Filtrage.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Ldef [(Var, Lexp)] Lexp
          deriving (Show, Eq)


---------------------------------------------------------------------------
-- Conversion de Sexp à Lexp                                             --
---------------------------------------------------------------------------

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
-- Cas de base
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (Scons Snil e) = s2l e -- éliminer les parenthèses inutiles


-- Abstraction currifiée
s2l (Scons (Scons (Scons Snil (Ssym "abs"))  -- (abs x1 e)
                  (Scons Snil (Ssym arg)))
            body) = Labs arg (s2l body)
-- élimination du sucre syntaxique:
-- (abs (x1 ... xn) e) ⇐⇒ (abs (x1) ... (abs (xn) e)..)
-- ⇐⇒ (abs (xn) ... (abs (x1) e)..)
s2l (Scons (Scons (Scons Snil (Ssym "abs")) 
                  (Scons xs (Scons Snil (Ssym argn))))
            body) = Labs argn (s2l 
                        (Scons (Scons (Scons Snil (Ssym "abs"))
                                      (xs))
                        body)
                    )


-- Ajout de déclarations locales
s2l (Scons (Scons (Scons Snil (Ssym "def")) ds) body) = let 
    defs (Scons (Scons Snil (Ssym arg)) defarg) = [(arg, s2l defarg)] -- (x e)
    defs (Scons (Scons (Scons Snil (Ssym arg)) xs) defarg) = let 
        abstraction = (Scons (Scons (Scons Snil (Ssym "abs"))(xs)) defarg)
        in [(arg, s2l abstraction)]              -- (x (x1...xn) e)
    defs (Scons Snil d) = defs d   -- parenthèses excessives
    defs (Scons ds' d) =  (defs ds') ++ (defs d)   -- (d1...dn)
    defs todef = error ("Expression Psil inconnue: " ++ showSexp todef)
    in Ldef (defs ds) (s2l body) 


-- Expression if 
-- élimination du sucre syntaxique: 
-- (if e et ee) ⇐⇒ (filter e (true: et) (false: ee))
s2l (Scons (Scons (Scons (Scons Snil (Ssym "if")) e) et) ee) = -- Scons Snil (Snum 6)
    s2l (Scons (Scons (Scons (Scons Snil (Ssym "filter")) e) 
                      (Scons (Scons Snil (Ssym "true:")) et)) 
                (Scons (Scons Snil (Ssym "false:")) ee))


-- Appel de constructeur, Filtrage, Appel de fonction currifié
s2l se = 
    let 
        identify (Scons (Scons Snil (Ssym "new")) c) = ("new", (c, []))
        identify (Scons (Scons Snil (Ssym "filter")) e) = ("filter", (e, []))
        identify (Scons rest e) = let intern = identify rest 
                                      left = fst intern
                                      right = snd intern
                                  in (left, (fst right, snd right ++ [e])) 
        identify _ = ("", ((Ssym ""), []))
    in case identify se of
        ("new", ((Ssym c), es)) -> if c /= "" then Lnew c (map s2l es) else
            error ("Expression Psil inconnue: " ++ (showSexp se))
        
        ("filter", (expr, bs)) -> Lfilter (s2l expr) (map branch bs) where
            branch (Scons (Scons Snil (Ssym "_")) e) = (Nothing, s2l e) 
            branch (Scons (Scons Snil (Ssym c)) e) = (Just (c, []), s2l e)
            branch (Scons filt e) = let 
                cons = extract filt
                extract (Scons (Scons Snil (Ssym c)) (Ssym x)) = (c, [x])
                extract (Scons rest (Ssym xn)) = let left = extract rest
                                                 in (fst left, snd left ++ [xn])
                extract (Scons Snil c) = extract c -- parenthèses excessives
                extract c = 
                    error ("Constructeur inconnu: " ++ (showSexp c))
                in (Just (fst cons, snd cons), s2l e)
            branch b = error ("Branche de filtrage inconnue: " ++ (showSexp b))
        
        ("", (_, _)) -> Lapply (s2l exprs) (s2l actual) 
            where
                parts = decompose se
                decompose (Scons (Scons Snil (Ssym fun)) e2) = (Ssym fun, e2)
                decompose (Scons es en) = (es, en)
                decompose _ = 
                    error ("Expression Psil inconnue: " ++ (showSexp se))
                exprs = fst parts
                actual = snd parts

        _ -> error ("Expression Psil inconnue: " ++ (showSexp se))


-- Expression inconnue 
-- s2l se = error ("Expression Psil inconnue: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs manipulées par l'évaluateur.
data Value = Vnum Int
           | Vcons Constructor [Value]
           | Vprim (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vcons s args) =
        showString "[" . showString s .
                   showValues (reverse args) . showString "]"
        where showValues [] = showString ""
              showValues (v:vs) = showString " " . showsPrec p v . showValues vs
    showsPrec _ (Vprim _) = showString "<function>"

type Env = [(Var, Value)]

valbool :: Bool -> Value
valbool x = if x then Vcons "true:"  [] else Vcons "false:" []

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = [("true", valbool True),
        ("false", valbool False),
        ("not", Vprim (\v -> case v of
                              Vcons "true:" [] -> valbool False
                              Vcons "false:" [] -> valbool True
                              _ -> error ("Pas un booléen: " ++ show v))),
        ("+", prim (+)),
        ("-", prim (-)),
        ("*", prim (*)),
        ("/", prim div),
        ("<" , primb (<)),
        ("<=" , primb (<=)),
        (">" , primb (>)),
        ("==" , primb (==)),
        (">=" , primb (>=))]
    where prim op = Vprim (\v1 -> case v1 of
                           Vnum x -> Vprim (\v2 -> case v2 of
                                            Vnum y -> Vnum (x `op` y)
                                            _ -> error ("Pas un entier: "
                                                       ++ show v2))
                           _ -> error ("Pas un entier: " ++ show v1))
          primb op = Vprim (\v1 -> case v1 of
                            Vnum x -> Vprim (\v2 -> case v2 of
                                             Vnum y -> valbool (x `op` y)
                                             _ -> error ("Pas un entier: "
                                                        ++ show v2))
                            _ -> error ("Pas un entier: " ++ show v1))

-- La fonction d'évaluation principale.
eval :: Env -> Lexp -> Value
eval _ (Lnum n) = Vnum n

eval env (Lvar var) = let 
    envLookup v [] = error ("Variable non définie: " ++ v)
    envLookup v (x:env') = if v == fst x then snd x else envLookup v env'
    in envLookup var env

eval env (Labs arg e) = Vprim (\x -> eval ((arg, x) : env) e)

eval env (Lapply fun actual) = case eval env fun of
    Vprim f -> f (eval env actual)
    _ -> error ("Une fonction était attendue: " ++ show (Lapply fun actual))

eval env (Lnew cons es) = Vcons cons (map (eval env) es)

eval env (Lfilter e []) = error ("Aucun filtre applicable: " ++ show (eval env e))
-- eval env (Lfilter e (b:bs)) = error (show (Lfilter e (b:bs)))
eval env (Lfilter e (b:bs)) = case b of
    (Nothing, epat) -> eval env epat
    (Just (cons, vs), epat) -> case eval env e of
        Vcons cons1 values -> if cons == cons1 && length vs == length values 
            then let env' = [(var, value) | var <- vs, value <- values] ++ env 
                 in eval env' epat 
            else eval env (Lfilter e bs)
        _ -> eval env (Lfilter e bs)

eval env (Ldef locals e) = let
    env' [] acc = acc
    env' (d:ds) acc = env' ds ((fst d, eval acc (snd d)):acc) 
    in eval (env' locals env) e


---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode 
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle
