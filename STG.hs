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

type Stack = [Value]

type Heap = U.Heap Closure

type Addr = U.Addr

data Code = DummyCode deriving (Show)

data Value = Addr
           | IntConst Int
           deriving (Show)
