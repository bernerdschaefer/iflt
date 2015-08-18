module TIM where
import Language
import qualified Utils as U

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
compile     :: CoreProgram -> State
eval        :: State -> [State]
showResults :: [State] -> String

runProg = showResults . eval . compile . parse

fullRun :: String -> String
fullRun = showFullResults . eval . compile . parse

showFullResults states
  = iDisplay (iConcat [
      iLayn (map showState states), iNewline, iNewline,
      showStats (last states)
    ])

data Instruction = Take Int
                 | Enter AMode
                 | Push AMode
                 deriving (Eq, Show)

data AMode = Arg Int
              | Label String
              | Code [Instruction]
              | IntConst Int
              deriving (Eq, Show)

-- intCode produces an empty code sequence
intCode = []

type State = ([Instruction], -- the current instruction stream
                 FramePtr,   -- address of current frame
                 Stack,      -- stack of arguments
                 ValueStack, -- value stack (not used yet)
                 Dump,       -- dump (not used yet)
                 Heap,       -- heap of frames
                 CodeStore,  -- labelled blocks of code
                 Stats)      -- statistics

data FramePtr = FrameAddr U.Addr  -- the address of a frame
                | FrameInt Int  -- an integer value
                | FrameNull     -- uninitialized
                deriving (Eq, Show)

type Stack = [Closure]
type Closure  = ([Instruction], FramePtr)

-- placeholder types for value stack and dump types
data ValueStack = DummyValueStack
data Dump = DummyDump

type Heap = U.Heap Frame

fAlloc   :: Heap -> [Closure] -> (Heap, FramePtr)
fGet     :: Heap -> FramePtr -> Int -> Closure
fUpdate  :: Heap -> FramePtr -> Int -> Closure -> Heap
fList    :: Frame -> [Closure] -- for printing

type Frame = [Closure]

fAlloc heap xs = (heap', FrameAddr addr)
                 where
                   (heap', addr) = U.hAlloc heap xs

fGet heap (FrameAddr addr) n = f !! (n-1)
                               where f = U.hLookup heap addr

fUpdate heap (FrameAddr addr) n closure
  = U.hUpdate heap addr newFrame
    where
      frame = U.hLookup heap addr
      newFrame = take (n-1) frame ++ [closure] ++ drop n frame

fList f = f

type CodeStore = U.ASSOC Name [Instruction]

codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l
  = U.aLookup cstore l (error ("Attempt to jump to unknown label "
                             ++ show l))

statInitial  :: Stats
statIncSteps :: Stats -> Stats
statGetSteps :: Stats -> Int

type Stats = Int
statInitial    = 0
statIncSteps s = s + 1
statGetSteps s = s

compile program
  = ([Enter (Label "main")],  -- initial instructions
     FrameNull,               -- null frame pointer
     initialArgStack,         -- argument stack
     initialValueStack,       -- value stack
     initialDump,             -- dump
     U.hInitial,              -- initial heap
     compiledCode,            -- compiled supercombinators
     statInitial)             -- initial statistics
       where
         scDefs         = preludeDefs ++ program
         compiledScDefs = map (compileSC initialEnv) scDefs
         compiledCode   = compiledScDefs ++ compiledPrimitives
         initialEnv = [ (name, Label name) | (name, args, body) <- scDefs ]
                      ++ [(name, Label name) | (name, code) <- compiledPrimitives ]

initialArgStack    = []
initialValueStack  = DummyValueStack
initialDump        = DummyDump
compiledPrimitives = []

type CompilerEnv = [(Name, AMode)]

compileSC :: CompilerEnv -> CoreScDefn -> (Name, [Instruction])
compileSC env (name, args, body)
  = (name, instructions args)
    where
      compiledBody = compileR body newEnv
      newEnv = (zip args (map Arg [1..])) ++ env
      instructions []   = compiledBody -- CAF optimization
      instructions args = Take (length args) : compiledBody

compileR :: CoreExpr -> CompilerEnv -> [Instruction]
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR (EVar v)    env = [Enter (compileA (EVar v) env)]
compileR (ENum n)    env = [Enter (compileA (ENum n) env)]
compileR e           env = error "compileR: unsupported call"

compileA :: CoreExpr -> CompilerEnv -> AMode
compileA (EVar v) env = U.aLookup env v (error ("unknown variable " ++ v))
compileA (ENum n) env = IntConst n
compileA e        env = Code (compileR e env)

eval state
  = state : restStates
    where
      restStates | final state = []
                 | otherwise   = eval nextState
      nextState  = doAdmin (step state)

doAdmin state = applyToStats statIncSteps state

final ([], frame, stack, vstack, dump, heap, cstore, stats) = True
final state                                                 = False

applyToStats f (instr, frame, stack, vstack, dump, heap, cstore, stats)
  = (instr, frame, stack, vstack, dump, heap, cstore, f stats)

step ((Take n:instr), fptr, stack, vstack, dump, heap, cstore, stats)
  | length stack >= n = (instr, fptr', (drop n stack), vstack, dump, heap', cstore, stats)
  | otherwise         = error "Too few args for Take"
    where
      (heap', fptr') = fAlloc heap (take n stack)

step ([Enter am], fptr, stack, vstack, dump, heap, cstore, stats)
  = (instr', fptr', stack, vstack, dump, heap, cstore, stats)
    where
      (instr', fptr') = amToClosure am fptr heap cstore

step ((Push am:instr), fptr, stack, vstack, dump, heap, cstore, stats)
  = (instr, fptr, stack', vstack, dump, heap, cstore, stats)
    where
      stack' = (amToClosure am fptr heap cstore) : stack

amToClosure (Arg n)      fptr heap cstore = fGet heap fptr n
amToClosure (Label l)    fptr heap cstore = (codeLookup cstore l, fptr)
amToClosure (Code i)     fptr heap cstore  = (i, fptr)
amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)

showResults states
  = iDisplay (iConcat [
      showState lastState, iNewline, iNewline, showStats lastState
    ]) where lastState = last states

showState (instr, fptr, stack, vstack, dump, heap, cstore, stats)
  = iConcat [
      iStr "Code:  ", showInstructions Terse instr, iNewline,
      showFrame heap fptr,
      showStack stack,
      showValueStack vstack,
      showDump dump,
      iNewline ]

showValueStack _ = iStr "[vstack]" `iAppend` iNewline
showDump _       = iStr "[dump]" `iAppend` iNewline

showFrame heap FrameNull = iStr "FramePtr (null)" `iAppend` iNewline
showFrame heap (FrameInt i)
  = iConcat [ iStr "FramePtr (int): ", iNum i, iNewline ]
showFrame heap (FrameAddr addr)
  = iConcat [ iStr "FramePtr (addr): <",
              iIndent (iInterleave iNewline
                                   (map showClosure (fList (U.hLookup heap addr)))),
              iStr ">", iNewline ]

showStack stack
  = iConcat [ iStr "Arg stack: [",
              iIndent (iInterleave iNewline (map showClosure stack)),
              iStr "]", iNewline ]

showClosure (i, f)
  = iConcat [ iStr "(", showInstructions Terse i, iStr ", ",
              showFramePtr f, iStr ")" ]

showFramePtr FrameNull = iStr "null"
showFramePtr (FrameAddr a) = iStr (show a)
showFramePtr (FrameInt n) = iStr "int " `iAppend` iNum n

showStats (instr, frame, stack, vstack, dump, heap, cstore, stats)
  = iConcat [ iStr "Steps take = ", iNum (statGetSteps stats), iNewline,
              iStr "No of frames allocated = ", iNum (U.hSize heap),
              iNum (statGetSteps stats) ]

data HowMuchToPrint = Full | Terse | None
showInstructions :: HowMuchToPrint -> [Instruction] -> Iseq
showInstructions None _ = iStr "{...}"
showInstructions Terse il
  = iConcat [iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"]
    where
      instrs = map (showInstruction None) il
      body | length il <= nTerse = instrs
           | otherwise           = (take nTerse instrs) ++ [iStr ".."]
showInstructions Full il
  = iConcat [iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"]
    where
      sep = iStr "," `iAppend` iNewline
      instrs = map (showInstruction Full) il

showInstruction d (Take m)  = (iStr "Take ")  `iAppend` (iNum m)
showInstruction d (Enter x) = (iStr "Enter ") `iAppend` (showArg d x)
showInstruction d (Push x)  = (iStr "Push ")  `iAppend` (showArg d x)

showArg d (Arg m)      = (iStr "Arg ")   `iAppend` (iNum m)
showArg d (Code il)    = (iStr "Code ")  `iAppend` (showInstructions d il)
showArg d (Label s)    = (iStr "Label ") `iAppend` (iStr s)
showArg d (IntConst n) = (iStr "IntConst ") `iAppend` (iNum n)

nTerse = 3