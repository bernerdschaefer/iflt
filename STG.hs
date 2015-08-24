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
             | ConApp Constr Atoms
             | PrimApp PrimOp Atoms
             | Literal Int

type Vars = [Var]
type Var = Name

type Literal = Int

data PrimOp = Add | Sub

type Constr = Int

type Alts = [Alt]
data Alt  = AlgAlt Constr Vars StgExpr
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
pprStgProgram program
  = iInterleave (iConcat [iStr ";", iNewline]) (map pprBind program)

pprBind :: Bind -> Iseq
pprBind (name, lf)
  = iConcat [ iStr name
            , iStr " "
            , pprLambdaForm lf ]

pprLambdaForm :: Lambda -> Iseq
pprLambdaForm (free, u, vars, body)
  = iConcat [ pprVars free
            , iStr " "
            , pprUpdateFlag u
            , iStr " "
            , pprVars vars
            , iStr " -> "
            , iNewline
            , iStr "  "
            , iIndent (pprStgExpr body) ]

pprStgExpr (Literal i)
  = pprLiteral i
pprStgExpr (App f args)
  = iConcat [ iStr "("
            , iStr f
            , iStr " "
            , iInterleave (iStr " ") (map pprAtom args)
            , iStr ")" ]

pprUpdateFlag Updateable    = iStr "u"
pprUpdateFlag NonUpdateable = iStr "n"

pprAtom (VarArg v) = pprVar v
pprAtom (LitArg i) = pprLiteral i

pprLiteral i = (iNum i) `iAppend` (iStr "#")

pprVars vars
  = iConcat [ iStr "{"
            , iInterleave (iStr ",") (map pprVar vars)
            , iStr "}" ]

pprVar var = (iStr var)

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
