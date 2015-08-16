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
