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
type CoreScDenf = ScDefn Name

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
