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
            | NInd Addr

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
      dispatch (NInd a)                  = indStep state a

-- a number at the head of the stack is an error
numStep :: TiState -> Int -> TiState
numStep state n = error "number applied as function"

-- unwind indirection
indStep (stack, dump, heap, globals, stats) a
  = (a : (tail stack), dump, heap, globals, stats)

-- unwind application
apStep (stack, dump, heap, globals, stats) a1 a2
  = (a1 : stack, dump, heap, globals, stats)

scStep (stack, dump, heap, globals, stats) sc args body
  = (newStack, dump, heap', globals, stats)
    where
      rootAddr = stack !! (length args)
      heap' = hUpdate newHeap rootAddr (NInd resultAddr)
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

instantiate (ELet rec defs body) heap env
  = instantiate body newHeap (env' ++ env)
    where
      insEnv | rec == True = env' ++ env
             | otherwise   = env

      (newHeap, env') = mapAccuml instantiateDef heap defs
      instantiateDef heap (name, rhs)
        = (heap', (name, addr)) where (heap', addr) = instantiate rhs heap insEnv

showResults states
  = iDisplay (iConcat [ iLayn (map showState states),
                        showStats (last states) ])

showState (stack, dump, heap, globals, stats)
  = iConcat [ showStack heap stack, iNewline ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack
  = iConcat [
      iStr "Stk [",
      iIndent (iInterleave iNewline (map showStackItem stack)),
      iStr "]"
    ]
    where
      showStackItem addr
        = iConcat [
            showFWAddr addr, iStr ": ",
            showStkNode heap (hLookup heap addr)
          ]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fAddr argAddr)
  = iInterleave (iStr " ") [ iStr "NAp",
                             showFWAddr fAddr,
                             showFWAddr argAddr,
                             iStr "(",
                             showNode (hLookup heap argAddr),
                             iStr ")" ]
showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NNum n) = iConcat [ iStr "NNum ", iNum n ]
showNode (NAp a1 a2)
  = iInterleave (iStr " ") [ iStr "NAp",
                             showAddr a1,
                             showAddr a2 ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NInd a) = iConcat [ iStr "NInd ", showAddr a ]

showAddr addr = iStr (show addr)

showFWAddr addr = iStr (space (4 - length str) ++ str)
                    where str = show addr

showStats (stack, dump, heap, globals, stats)
  = iConcat [ iNewline, iNewline, iStr "Total number of steps = ",
              iNum (tiStatGetSteps stats) ]

exercise_2_10
  = putStrLn $ showResults $ eval $ compile $ parse
      "f x y = let                          \n\
      \          a = x ;                    \n\
      \          b = y                      \n\
      \        in                           \n\
      \        a ;                          \n\
      \main = f 3 4                           "

exercise_2_11
  = putStrLn $ showResults $ eval $ compile $ parse
      "pair x y f = f x y ;                 \n\
      \fst p = p K ;                        \n\
      \snd p = p K1 ;                       \n\
      \f x y = letrec                       \n\
      \          a = pair x b ;             \n\
      \          b = pair y a               \n\
      \        in                           \n\
      \        fst (snd (snd (snd a))) ;    \n\
      \main = f 3 4                           "

exercise_2_12
  = putStrLn $ showResults $ eval $ compile $ parse
      "main = letrec f = f x in f"

-- mark1: twice twice       = 33  steps
--        twice twice twice = 165 steps
-- mark3: twice twice       = 32  steps
--        twice twice twice = 138 steps
exercise_2_13
  = putStrLn $ showResults $ eval $ compile $ parse
      "id x = x ; \n\
      \main = twice twice twice id 3"
