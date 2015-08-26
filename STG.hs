module STG where
import Language
import qualified Utils as U

--
-- Syntax
--

type StgProgram = Binds

type Binds = [Bind]
type Bind  = (Name, Lambda)

data StgExpr = Let IsRec Binds StgExpr
             | Case StgExpr Alts
             | App Var Atoms
             | PackApp Pack Atoms
             | ConApp Constr Atoms
             | PrimApp PrimOp Atoms
             | Literal Int

type Vars = [Var]
type Var = Name

type Literal = Int

data PrimOp = Add | Sub

type Constr = Name
data Pack   = Pack Int Int

type Alts = [Alt]
data Alt  = AlgAlt Constr Vars StgExpr
          | PackAlt Int Vars StgExpr
          | PrimAlt Literal StgExpr
          | NormAlt Var StgExpr
          | DefaultAlt StgExpr

type Atoms = [Atom]
data Atom  = VarArg Var
           | LitArg Literal

type Lambda = (Vars, UpdateFlag, Vars, StgExpr)

data UpdateFlag = Updateable | NonUpdateable

--
-- Printing
--

pprStgProgram :: StgProgram -> Iseq
pprStgProgram program = pprBinds program

pprBinds binds
  = iInterleave (iConcat [iStr ";", iNewline]) (map pprBind binds)

pprBind :: Bind -> Iseq
pprBind (name, lf)
  = iConcat [ iStr name
            , iStr " = "
            , pprLambdaForm lf ]

pprLambdaForm :: Lambda -> Iseq
pprLambdaForm (free, u, vars, body)
  = iConcat [ pprVars free
            , iStr " "
            , pprUpdateFlag u
            , iStr " "
            , pprVars vars
            , iStr " ->"
            , iNewline
            , iStr "  "
            , iIndent (pprStgExpr body) ]

pprStgExpr (Literal i) = pprLiteral i

pprStgExpr (Let isrec binds body)
  = iConcat [ iStr keyword
            , iNewline
            , iStr "  "
            , iIndent (pprBinds binds)
            , iNewline
            , iStr "in "
            , pprStgExpr body ]
    where
      keyword | isrec == True = "letrec"
              | otherwise     = "let"

pprStgExpr (Case expr alts)
  = iConcat [ iStr "case "
            , pprStgExpr expr
            , iStr " of"
            , iNewline
            , iStr "  "
            , iIndent (pprStgAlts alts) ]

pprStgExpr (App f args)
  = iConcat [ iStr f
            , iStr " "
            , pprAtoms args ]

pprStgExpr (ConApp constr args)
  = iConcat [ iStr constr
            , iStr " "
            , pprAtoms args ]

pprStgExpr (PackApp pack args)
  = iConcat [ pprPack pack
            , iStr " "
            , pprAtoms args ]

pprPack (Pack tag arity)
  = iConcat [ iStr "Pack{"
            , iNum tag
            , iStr ","
            , iNum arity
            , iStr "}" ]

pprUpdateFlag Updateable    = iStr "u"
pprUpdateFlag NonUpdateable = iStr "n"

pprStgAlts alts = iInterleave (iStr ";" `iAppend` iNewline) (map pprAlt alts)

pprAlt (AlgAlt constr vars expr)
  = iConcat [ iStr constr
            , iStr " "
            , pprVars vars
            , iStr " -> "
            , pprStgExpr expr ]

pprAlt (PackAlt tag vars expr)
  = iConcat [ iStr "<"
            , iNum tag
            , iStr "> "
            , pprVars vars
            , iStr " -> "
            , pprStgExpr expr ]

pprAtoms args
  = iConcat [ iStr "{"
            , iInterleave (iStr " ") (map pprAtom args)
            , iStr "}" ]

pprAtom (VarArg v) = pprVar v
pprAtom (LitArg i) = pprLiteral i

pprLiteral i = (iNum i) `iAppend` (iStr "#")

pprVars vars
  = iConcat [ iStr "{"
            , iInterleave (iStr ",") (map pprVar vars)
            , iStr "}" ]

pprVar var = (iStr var)

--
-- Transformation from Core to STG
--

transformCoreProgram :: CoreProgram -> StgProgram
transformCoreProgram program
  = map transformCoreScDefn program

transformCoreScDefn :: CoreScDefn -> Bind
transformCoreScDefn (name, args, body)
  = (name, ([], NonUpdateable, args, transformCoreExpr body'))
    where
      body' = coreExprToANF body

transformCoreExpr expr@(EAp _ _)
  = transformCoreApp f args
    where (f, args) = flattenAp expr

transformCoreExpr (ECase e alts)
  = (Case (transformCoreExpr e) (transformCoreAlts alts))

transformCoreExpr (EVar v)
  = transformCoreApp v []

transformCoreExpr (EConstr tag arity@0)
  = PackApp (Pack tag arity) []

transformCoreExpr e = error ("unknown expression " ++ (show e))

transformCoreApp :: Var -> [CoreExpr] -> StgExpr
transformCoreApp f args
  = (App f atoms)
    where
      atoms = map atom args
      atom (EVar v) = VarArg v
      atom (ENum n) = LitArg n
      atom e        = error ("not an atom: " ++ (show e))

flattenAp :: CoreExpr -> (Var, [CoreExpr])
flattenAp expr
  = flatten expr []
    where
      flatten (EVar v)      args = (v, args)
      flatten (EAp f arg)   args = flatten f (arg:args)
      flatten expr          args = error ("unknown expr " ++ (show expr))

transformCoreAlts :: [CoreAlt] -> Alts
transformCoreAlts coreAlts
  = alts
    where
      alts = map transformCoreAlt coreAlts

transformCoreAlt :: CoreAlt -> Alt
transformCoreAlt (tag, vars, body)
  = PackAlt tag vars (transformCoreExpr body)

coreExprToANF :: CoreExpr -> CoreExpr
coreExprToANF expr
  = anfExpr
    where
      maybeLet e []    = e
      maybeLet e defns = ELet nonRecursive defns e

      (expr', defns) = coreExprToANF' expr []

      anfExpr = maybeLet expr' defns

coreExprToANF' :: CoreExpr -> [CoreDefn] -> (CoreExpr, [CoreDefn])
coreExprToANF' expr@(EAp f a) defns = coreApToANF f a defns
coreExprToANF' e              defns = (e, defns)

coreApToANF :: CoreExpr -> CoreExpr -> [CoreDefn] -> (CoreExpr, [CoreDefn])

coreApToANF f (ENum n) defns
  = (EAp f' (ENum n), defns')
    where (f', defns') = coreExprToANF' f defns

coreApToANF f (EVar v) defns
  = (EAp f' (EVar v), defns')
    where (f', defns') = coreExprToANF' f defns

coreApToANF f a defns
  = (EAp f' (EVar id), defns3)
    where
      id = newId defns
      defns1 = (id, a') : defns
      (f', defns2) = coreExprToANF' f defns1
      (a', defns3) = coreExprToANF' a defns2

newId :: [CoreDefn] -> Name
newId defns = "**x" ++ (show (length defns)) ++ "**"
--
-- Evaluation
--

data State = State { code :: Code
                   , args :: Stack
                   , rets :: [Continuation]
                   , upds :: [UpdateFrame]
                   , heap :: Heap
                   , env :: [(Name, Addr)]
                   }

data UpdateFrame  = DummyFrame
data Continuation = DummyContinuation

data Closure = Closure { vars :: [Name]
                       , updateable :: Bool
                       , xs :: [Int]
                       , body :: Code
                       , varValues :: [Int]
                       }

type GlobalEnv = [(Name, Addr)]
type LocalEnv = [(Name, Value)]

type Stack = [Value]

type Heap = U.Heap Closure

type Addr = U.Addr

type Code = [Instruction]

data Value = Addr
           | IntConst Int

data Instruction = Eval StgExpr LocalEnv
                 | Enter Addr
                 | ReturnCon Int [Value]
                 | ReturnInt Int


vals local global [] = []
vals local global (x:xs) = (val local global x) : (vals local global xs)

val local global (ENum n) = IntConst n
val local global (EVar v)
  = U.aLookup local v (U.aLookup global v (error ("unknown variable " ++ v)))

initialState
  = State { code = [Eval (App "main" []) []]
          , args = []
          , rets = []
          , upds = []
          , heap = U.hInitial
          , env  = []
          }
