module GM where
import Language
import qualified Utils as U

data State = State { code :: Code
                   , args :: Stack
                   , rets :: [Continuation]
                   , upds :: [UpdateFrame]
                   , heap :: Heap
                   , env :: [(Name, Addr)]
                   } deriving (Show)

data UpdateFrame  = DummyFrame deriving (Show)
data Continuation = DummyContinuation deriving (Show)

data Closure = Closure { vars :: [Name]
                       , updateable :: Bool
                       , xs :: [Int]
                       , body :: Code
                       , varValues :: [Int]
                       } deriving (Show)

type GlobalEnv = [(Name, Addr)]
type LocalEnv = [(Name, Value)]

type Stack = [Value]

type Heap = U.Heap Closure

type Addr = U.Addr

type Code = [Instruction]

data Value = Addr
           | IntConst Int
           deriving (Show)

data Instruction = Eval CoreExpr LocalEnv
                 | Enter Addr
                 | ReturnCon Int [Value]
                 | ReturnInt Int
                 deriving (Show)

vals local global [] = []
vals local global (x:xs) = (val local global x) : (vals local global xs)

val local global (ENum n) = IntConst n
val local global (EVar v)
  = U.aLookup local v (U.aLookup global v (error ("unknown variable " ++ v)))

initialState
  = State { code = [Eval (EVar "main") []]
          , args = []
          , rets = []
          , upds = []
          , heap = U.hInitial
          , env  = []
          }
