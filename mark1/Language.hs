module Language where

-- Section 1.3: Data types

data Expr a
  = EVar Name -- Variables
  | ENum Int  -- Numbers
  | EConstr Int Int -- Constructor tag arity
  | EAp (Expr a) (Expr a) -- Applications
  | ELet            -- Let(rec) expressions
      IsRec         -- boolean with True = recursive
      [(a, Expr a)] -- Definitions
      (Expr a)      -- Body of let(rec)
  | ECase           -- Case expression
    (Expr a)        -- Expression to scrutinize
    [Alter a]       -- Alternatives
  | ELam [a] (Expr a) -- Lambda abstractions
  deriving (Show)

-- use Name as the binding type for Expr
type CoreExpr = Expr Name

-- A variable's name is a list of characters
type Name = String

-- IsRec is represented as a boolean,
-- where recursive is true and nonRecursive is false.
type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

-- pick out variables bound by definitions
bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]
--
-- pick out right-hand-sides of definitions
rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

-- an alternative consists of a tag,
-- a list of the bound variables,
-- and the expression to the right of the arrow
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- helper function to identify expressions
-- with no internal structure
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

-- A Core-language program is a list
-- of supercombinator definitions.
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- A supercombinator definition contains
-- the name of the supercombinator,
-- its arguments,
-- and its body
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- representation of the program:
--    main = double 21 ;
--    double x = x+x
exampleSection13 :: CoreProgram
exampleSection13 =
  [("main", [], (EAp (EVar "double") (ENum 21))),
   ("double", ["x"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "x")))
  ]

-- Section 1.4: a small standard prelude

preludeDefs :: CoreProgram
preludeDefs
  = [
      -- I x = x ;
      ("I", ["x"], EVar "x"),
      -- K x y = x ;
      ("K", ["x", "y"], EVar "x"),
       -- K1 x y = y ;
      ("K1", ["x", "y"], EVar "y"),
      -- S f g x = f x (g x) ;
      ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                 (EAp (EVar "g") (EVar "x"))),
      -- compose f g x = f (g x) ;
      ("compose", ["f", "g", "x"], EAp (EVar "f")
                                       (EAp (EVar "g") (EVar "x"))),
      -- twice f = compose f f ;
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]

-- Section 1.5: a pretty printer

iNil     :: Iseq -- the empty Iseq
iStr     :: String -> Iseq
iAppend  :: Iseq -> Iseq -> Iseq -- append two iseqs
iNewline :: Iseq                 -- newline with indentation
iIndent  :: Iseq -> Iseq         -- indent an iseq
iDisplay :: Iseq -> String       -- turn an iseq into a string

pprAExpr :: CoreExpr -> Iseq
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = iConcat [ iStr "(", pprExpr e, iStr ")" ]

pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (ENum n) = iStr (show n)
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline,
              iStr "  ", iIndent (pprDefns defns), iNewline,
              iStr "in ", pprExpr expr ]
    where
      keyword | not isrec = "let"
              | isrec     = "letrec"
-- TODO: EConstr
-- TODO: ECase
-- TODO: ELam

pprDefns :: [(Name,CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
                 where
                   sep = iConcat [ iStr ";", iNewline ]
pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

iConcat :: [Iseq] -> Iseq
iConcat [] = iNil
iConcat (s : seqs) = iAppend s (iConcat seqs)

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep []         = iNil
iInterleave seq (s : [])   = s
iInterleave sep (s : seqs) = s `iAppend` sep `iAppend` (iInterleave sep seqs)

pprScDefn :: CoreScDefn -> Iseq
pprScDefn (name, [], body) =
  iConcat [ iStr name, iStr " = ", pprExpr(body) ]
pprScDefn (name, vars, body) =
  iConcat [ iStr name,
            iStr " ",
            iInterleave (iStr " ") (map iStr vars),
            iStr " = ",
            pprExpr(body) ]

pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave iNewline (map pprScDefn prog)

pprint prog = iDisplay (pprProgram prog)

-- Section 1.5.3: implement iseq

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iNil              = INil
iAppend seq1 seq2 = IAppend seq1 seq2
-- TODO: exercise 1.7: handle newlines in iStr
iStr str          = IStr str
iIndent seq       = IIndent seq
iNewline          = INewline

flatten :: Int               -- current column (0 for first)
            -> [(Iseq, Int)] -- work list
            -> String        -- result
flatten col [] = ""
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col (((IStr s), _) : seqs) = s ++ (flatten col seqs)
flatten col (((IAppend seq1 seq2), _) : seqs) = flatten col ((seq1, col) : (seq2, col) : seqs)
flatten col ((INewline, indent) : seqs)
  = '\n' : (space indent) ++ (flatten indent seqs)
flatten col ((IIndent seq, indent) : seqs)
  = flatten col ((seq, col) : seqs)

iDisplay seq = flatten 0 [(seq, 0)]

space n = take n (repeat ' ')

exampleSection154 = pprint [
  ("x", [], (ELet recursive [("a", (ENum 2)), ("b", (ENum 3))]
              (EAp (EAp (EVar "+") (EVar "a")) (EVar "b"))))
  ]

-- Section 1.6: Parser for Core Language

-- lex input into tokens
clex :: String -> [Token]

-- perform syntactic analysis of tokens
-- syntax :: [Token] -> CoreProgram

-- parse :: String -> CoreProgram
-- parse = syntax . clex -- why not syntax . clex . read ?

type Token = String -- a token is a non-empty string

clex (c:cs) | isWhitespace c = clex cs

clex (c:cs) | isAlpha c = varToken : clex restCs
      where
        varToken = c : takeWhile isIdChar cs
        restCs = dropWhile isIdChar cs

clex (c:cs) | isDigit c = numToken : clex restCs
      where
        numToken = c : takeWhile isDigit cs
        restCs = dropWhile isDigit cs

clex (c:cs) = [c] : clex cs

clex [] = []

isWhitespace ' '  = True
isWhitespace '\r' = True
isWhitespace '\n' = True
isWhitespace '\t' = True
isWhitespace _    = False

isDigit x = x >= '0' && x <= '9'

isAlpha x = x >= 'a' && x <= 'z' ||
            x >= 'A' && x <= 'Z'

isIdChar x
  = isAlpha x || isDigit x || (x == '_')

-- a parser for the type a
-- takes a list of tokens
-- and returns a list of parses,
-- each of which consists
-- of a value a
-- paired with the remaining tokens
type Parser a = [Token] -> [(a, [Token])]

-- pLit (short for literal)
-- takes a string
-- and returns a parser
-- which recognizes tokens containing that string,
-- returning the same string as the value.
pLit :: String -> Parser String
pLit s (tok:toks)
  | s == tok  = [(s, toks)]
  | otherwise = []
pLit s [] = []

-- pVar decides whether a token is a variable
pVar :: Parser String
pVar [] = []
pVar (tok:toks)
  | isAlpha (head tok) = [(tok, toks)]
  | otherwise          = []

-- pAlt combines two parsers by calling each
-- and combining their parses.
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

-- pThen combines two parsers
-- by first parsing with p1
-- then parsing a seconding value from the input with p2.
-- The third argument combines the two results
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks,
                              (v2, toks2) <- p2 toks1 ]

pGreeting :: Parser (String, String)
pGreeting = pThen3 makeGreeting
                    pHelloOrGoodbye
                    pVar
                    (pLit "!")
              where
                makeGreeting hg name excl = (hg, name)

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks
  = [ (combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks,
                                  (v2, toks2) <- p2 toks1,
                                  (v3, toks3) <- p3 toks2 ]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks
  = [ (combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks,
                                      (v2, toks2) <- p2 toks1,
                                      (v3, toks3) <- p3 toks2,
                                      (v4, toks4) <- p4 toks3 ]