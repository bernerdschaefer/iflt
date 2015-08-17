module TIM where
import Language
import Utils

-- Transition rules
--
--    The state of the machine is the quintuple:
--
--      (instructions, frame pointer, stack, heap, code store)
--      (i, f, s, h, c)
--
--    Take n makes the top n stack elements into a new frame
--    and makes the current frame pointer point to it.
--
--    4.1   Take N : i  f  c1 : ... : cN : s  h                    c
--    ==>            i  f'                 s  h[f' : <c1,...,cN>]  c
--
--    Push (Arg K) fetches the Kth closure from the current frame
--    and pushes it onto the stack.
--    4.2   Push (Arg K) : i  f              s  h[f : <(i1, f1),...,(iK, fK),...,(iN, fN)>] c
--    ==>                  i  f   (iK, fK) : s  h                                           c
--
--    Push (Label L) looks up the label L
--    and pushes a closure
--    consisting of the code pointer with the current frame pointer.
--    4.3   Push (Label L) : i  f            s  h  c[l : i']
--    ==>                    i  f  (i', f) : s  h  c
--
--    Push (Code i') makes the target code sequence i
--    part of the instruction itself.
--    4.4   Push (Code i') : i  f            s  h  c
--    ==>                    i  f  (i', f) : s  h  c
--
--    Push (IntConst N) pushes a special closure.
--    4.5   Push (IntConst N) : i  f                 s  h  c
--    ==>                       i  f  (intCode, N) : s  h  c
--
--    4.6   [Enter (Label L)]  f  s  h  c[L : i]
--    ==>                   i  f  s  h  c
--
--    4.7   [Enter (Arg K)]  f   s  h[f : <(i1, f1),...,(iK, fK),...,(iN, fN)>]  c
--    ==>                iK  fK  s  h                                            c
--
--    4.8   [Enter (Code i)]  f  s  h  c
--    ==>                  i  f  s  h  c
--
--    4.9   [Enter (IntConst N)]  f  s  h  c
--    ==>                intCode  N  s  h  c

runProg     :: String -> String
compile     :: CoreProgram -> TimState
eval        :: TimState -> [TimState]
showResults :: [TimState] -> String

runProg = showResults . eval . compile . parse

fullRun :: String -> String
fullRun = showFullResults . eval . compile . parse

eval _            = error "not yet implemented"
showResults _     = error "not yet implemented"
showFullResults _ = error "not yet implemented"

data Instruction = Take Int
                 | Enter TimAMode
                 | Push TimAMode

data TimAMode = Arg Int
              | Label String
              | Code [Instruction]
              | IntConst Int

-- intCode produces an empty code sequence
intCode = []

type TimState = ([Instruction], -- the current instruction stream
                 FramePtr,      -- address of current frame
                 TimStack,      -- stack of arguments
                 TimValueStack, -- value stack (not used yet)
                 TimDump,       -- dump (not used yet)
                 TimHeap,       -- heap of frames
                 CodeStore,     -- labelled blocks of code
                 TimStats)      -- statistics

data FramePtr = FrameAddr Addr  -- the address of a frame
                | FrameInt Int  -- an integer value
                | FrameNull     -- uninitialized

type TimStack = [Closure]
type Closure  = ([Instruction], FramePtr)

-- placeholder types for value stack and dump types
data TimValueStack = DummyTimValueStack
data TimDump = DummyTimDump

type TimHeap = Heap Frame

fAlloc   :: TimHeap -> [Closure] -> (TimHeap, FramePtr)
fGet     :: TimHeap -> FramePtr -> Int -> Closure
fUpdate  :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fList    :: Frame -> [Closure] -- for printing

type Frame = [Closure]

fAlloc heap xs = (heap', FrameAddr addr)
                 where
                   (heap', addr) = hAlloc heap xs

fGet heap (FrameAddr addr) n = f !! (n-1)
                               where f = hLookup heap addr

fUpdate heap (FrameAddr addr) n closure
  = hUpdate heap addr newFrame
    where
      frame = hLookup heap addr
      newFrame = take (n-1) frame ++ [closure] ++ drop n frame

fList f = f

type CodeStore = ASSOC Name [Instruction]

codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l
  = aLookup cstore l (error ("Attempt to jump to unknown label "
                             ++ show l))

statInitial  :: TimStats
statIncSteps :: TimStats -> TimStats
statGetSteps :: TimStats -> Int

type TimStats = Int
statInitial    = 0
statIncSteps s = s + 1
statGetSteps s = s

compile program
  = ([Enter (Label "main")],  -- initial instructions
     FrameNull,               -- null frame pointer
     initialArgStack,         -- argument stack
     initialValueStack,       -- value stack
     initialDump,             -- dump
     hInitial,                -- initial heap
     compiledCode,            -- compiled supercombinators
     statInitial)             -- initial statistics
       where
         scDefs         = preludeDefs ++ program
         compiledScDefs = map (compileSC initialEnv) scDefs
         compiledCode   = compiledScDefs ++ compiledPrimitives
         initialEnv = [ (name, Label name) | (name, args, body) <- scDefs ]
                      ++ [(name, Label name) | (name, code) <- compiledPrimitives ]

initialArgStack    = []
initialValueStack  = DummyTimValueStack
initialDump        = DummyTimDump
compiledPrimitives = []

type TimCompilerEnv = [(Name, TimAMode)]

compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])
compileSC env (name, args, body)
  = (name, Take (length args) : instructions)
    where
      instructions = compileR body newEnv
      newEnv = (zip args (map Arg [1..])) ++ env

compileR :: CoreExpr -> TimCompilerEnv -> [Instruction]
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR (EVar v)    env = [Enter (compileA (EVar v) env)]
compileR (ENum n)    env = [Enter (compileA (ENum n) env)]
compileR e           env = error "compileR: unsupported call"

compileA :: CoreExpr -> TimCompilerEnv -> TimAMode
compileA (EVar v) env = aLookup env v (error ("unknown variable " ++ v))
compileA (ENum n) env = IntConst n
compileA e        env = Code (compileR e env)
