import Language
import Utils

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

-- the spine stack is a stack of heap addresses
type TiStack = [Addr]

-- the dump is not yet needed,
-- so provide a summy implementation
data TiDump = DummyTiDump
initialTiDump = DummyTiDump

-- the heap is a heap of nodes
type TiHeap = Heap Node

data Node = NAp Addr Addr
            | NSupercomb Name [Name] CoreExpr
            | NNum Int

type TiGlobals = ASSOC Name Addr

-- for capturing runtime statistics of the machine,
-- in this case the number of executed steps
type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial     = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s+1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

-- applies a function to stats portion
-- of a state.
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, globals, stats)
  = (stack, dump, heap, globals, f stats)

-- the compiler takes a program
-- and creates the initial state for it.
compile program
  = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
    where
      scDefs = program ++ preludeDefs ++ extraPreludeDefs

      (initialHeap, globals) = buildInitialHeap scDefs

      initialStack = [addressOfMain]
      addressOfMain = aLookup globals "main" (error "main is not defined")

extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = mapAccuml allocateSc hInitial scDefs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
  = (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)

eval state = state : restStates
               where
                 restStates
                   | tiFinal state = []
                   | otherwise     = eval nextState

                 nextState = doAdmin (step state)

doAdmin state = applyToStats tiStatIncSteps state

-- the state is final if the stack
-- contains a single item
-- which is either a number or data object
tiFinal :: TiState -> Bool
tiFinal ([addr], dump, heap, globals, stats)
  = isDataNode (hLookup heap addr)
tiFinal ([], dump, heap, globals, stats)
  = error "Empty stack"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode _        = False

step :: TiState -> TiState
step state
  = dispatch (hLookup heap (head stack))
    where
      (stack, dump, heap, globals, stats) = state

      dispatch (NNum n)                  = numStep state n
      dispatch (NAp a1 a2)               = apStep  state a1 a2
      dispatch (NSupercomb sc args body) = scStep  state sc args body

-- a number at the head of the stack is an error
numStep :: TiState -> Int -> TiState
numStep state n = error "number applied as function"

-- unwind application
apStep (stack, dump, heap, globals, stats) a1 a2
  = (a1 : stack, dump, heap, globals, stats)

scStep (stack, dump, heap, globals, stats) sc args body
  = (newStack, dump, newHeap, globals, stats)
    where
      newStack = resultAddr : (drop (length args+1) stack)

      (newHeap, resultAddr) = instantiate body heap env
      env = argBindings ++ globals
      argBindings = zip2 args (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc : stack)
  = map getarg stack
    where
      getarg addr = arg where (NAp f arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)

instantiate (ENum n) heap env = hAlloc heap (NNum n)

instantiate (EAp e1 e2) heap env
  = hAlloc heap2 (NAp a1 a2) where (heap1, a1) = instantiate e1 heap  env
                                   (heap2, a2) = instantiate e2 heap1 env

instantiate (EVar v) heap env
  = (heap, aLookup env v (error ("Undefined name " ++ show v)))

showResults states
  = iDisplay (iConcat [ iLayn (map showState states),
                        showStats (last states) ])

showState (stack, dump, heap, globals, stats)
  = iConcat [ showStack heap stack, iNewline ]
