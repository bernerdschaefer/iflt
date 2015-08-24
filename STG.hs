module STG where
import Language
import qualified Utils as U

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

type Program = Binds

type Binds = [Bind]

type Bind = (Name, Lambda)

data UpdateFlag = Updateable | NonUpdateable

data StgExpr = Let Binds StgExpr
             | Letrec Binds StgExpr
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

type Atoms = [Atom]

type Alts = [Alt]

data Alt = AlgAlt Constr Vars StgExpr
         | PrimAlt Literal StgExpr
         | NormAlt Var StgExpr
         | DefaultAlt StgExpr

data Atom = VarArg Name
          | LitArg Literal

type Lambda = ( [Name]     -- free variables
                , UpdateFlag     -- updateable
                , [Name]   -- arguments
                , StgExpr -- body
                )

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