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
--    Take t n allocates a frame of size t,
--    takes n closures from the stack,
--    and puts them into the first n locations of the frame.
--
--    4.1   Take T N : i  f  c1 : ... : cN : s  h                    c
--    ==>              i  f'                 s  h[f' : <c1,...,cN>]  c
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
--
--    4.10  Op Sub : i  f  s    n1 : n2 : v  h  c
--    ==>            i  f  s  (n1 - n2) : v  h  c
--
--    4.11  [Return]  f   (i',f') : s  v  h  c
--    ==>         i'  f'            s  v  h  c
--
--    PushV pushes the number masquerading as the frame pointer
--    to the top of the value stack.
--
--    4.12  PushV FramePtr : i  n  s      v  h  c
--      =>                   i  n  s  n : v  h  c
--
--    4.13  [Cond i1 i2]  f  s  0 : v  h  c
--      =>            i1  f  s      v  h  c
--          [Cond i1 i2]  f  s  n : v  h  c
--      =>            i2  f  s      v  h  c
--
--    4.14  PushV (IntVConst n) : i  f  s      v  h  c
--      =>                        i  f  s  n : v  h  c
--
--    Move i a moves the new closure a
--    into slot i of the current frame.
--
--    4.15  (Move i a) : i  f  s  v  h              c
--      =>               i  f  s  v  h[f : <i: a>]  c

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

data Instruction = Take Int Int
                 | Enter AMode
                 | Push AMode
                 | PushV ValueAMode
                 | Op Op
                 | Cond [Instruction] [Instruction]
                 | Return
                 | Move Int AMode
                 deriving (Eq, Show)

data Op = Add | Sub | Mult | Div | Neg
        | Gr | GrEq | Lt | LtEq | Eq | NotEq
        deriving (Eq, Show)

data AMode = Arg Int
              | Label String
              | Code [Instruction]
              | IntConst Int
              deriving (Eq, Show)

data ValueAMode = FramePtr
                | IntVConst Int
                deriving (Eq, Show)

-- push the integer masquerading as a frame pointer and return
intCode = [PushV FramePtr, Return]

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

type ValueStack = [Int]

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

data Stats = Stats { stepCount :: Int
                   , maxStackDepth :: Int
                   } deriving (Show)

statInitial    = Stats { stepCount = 0, maxStackDepth = 0 }
statIncSteps stats = stats { stepCount = (stepCount stats) + 1 }
statGetSteps stats = stepCount stats
statUpdateMaxStackDepth stack stats
  | (length stack) > (maxStackDepth stats) = stats { maxStackDepth = (length stack) }
  | otherwise                              = stats

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

initialArgStack    = [([], FrameNull)]
initialValueStack  = []
initialDump        = DummyDump
compiledPrimitives = [
    ("+", mkArithOp (Op Add)),
    ("-", mkArithOp (Op Sub)),
    ("*", mkArithOp (Op Mult)),
    ("/", mkArithOp (Op Div)),
    ("<", mkArithOp (Op Lt)),
    ("if", compiledIf)
  ]

mkArithOp (Op op) = [ Take 2 2,
                      Push (Code [ Push (Code [Op op, Return]), Enter (Arg 1) ]),
                      Enter (Arg 2) ]

compiledIf
  = [ Take 3 3,
      Push (Code [ Cond [Enter (Arg 2)] [Enter (Arg 3)] ]),
      Enter (Arg 1) ]

type CompilerEnv = [(Name, AMode)]

compileSC :: CompilerEnv -> CoreScDefn -> (Name, [Instruction])
compileSC env (name, args, body)
  = (name, instructions args)
    where
      n = (length args)
      (d', compiledBody) = compileR body newEnv n
      newEnv = (zip args (map Arg [1..])) ++ env
      instructions []   = compiledBody -- CAF optimization
      instructions args = Take d' n : compiledBody

compileR :: CoreExpr -> CompilerEnv -> Int -> (Int, [Instruction])

compileR (EAp (EAp (EAp (EVar "if") n) e1) e2) env d
  = (d3, [Push (Code [Cond i1 i2]), Enter am])
    where
      (d1, i1) = compileR e1 env d
      (d2, i2) = compileR e2 env d1
      (d3, am) = compileA n env d2

compileR (EAp (EAp (EVar "<") e1) e2) env d = compileB (EAp (EAp (EVar "<") e1) e2) env d [Return]
compileR (EAp (EAp (EVar "+") e1) e2) env d = compileB (EAp (EAp (EVar "+") e1) e2) env d [Return]

compileR (ENum n) env d = (d', is)
  where (d', is) = compileB (ENum n) env d [Return]

compileR (ELet isRec defns body) env d
  = (d', moves ++ is)
    where
      ((dn, env'), moves) = U.mapAccuml makeMove (d, env) defns
      (d', is) = compileR body env' dn

      makeMove (d, env) (name, e) = ((d', env'), Move (d + 1) am)
        where
          compileEnv | isRec == True = env'
                     | otherwise = env
          (d', am) = compileA e compileEnv (d + 1)
          env' = env ++ [(name, (Arg (d + 1)))]

compileR (EAp e1 e2) env d = (d2, Push am : is)
  where
    (d1, am) = compileA e2 env d
    (d2, is) = compileR e1 env d1

compileR (EVar v) env d = (d', [Enter is])
  where (d', is) = compileA (EVar v) env d

compileR e env d = error ("compileR: unsupported call " ++ (show e))

compileA :: CoreExpr -> CompilerEnv -> Int -> (Int, AMode)
compileA (EVar v) env d = (d, U.aLookup env v (error ("unknown variable " ++ v)))
compileA (ENum n) env d = (d, IntConst n)
compileA e        env d = (d', Code is)
  where (d', is) = compileR e env d

compileB :: CoreExpr -> CompilerEnv -> Int -> [Instruction] -> (Int, [Instruction])
compileB (ENum n) env d cont = (d, PushV (IntVConst n) : cont)
compileB (EAp (EAp (EVar "+") e1) e2) env d cont = (d2, i2)
  where
    (d1, i1) = compileB e1 env d  (Op Add : cont)
    (d2, i2) = compileB e2 env d1 i1
compileB (EAp (EAp (EVar "<") e1) e2) env d cont = (d2, i2)
  where
    (d1, i1) = compileB e1 env d  (Op Lt : cont)
    (d2, i2) = compileB e2 env d1 i1
compileB e env d cont = (d', Push (Code cont) : is)
  where (d', is) = (compileR e env d)

eval state
  = state : restStates
    where
      restStates | final state = []
                 | otherwise   = eval nextState
      nextState  = doAdmin (step state)

doAdmin state
  = applyToStats (statUpdateMaxStackDepth stack) state2
    where
      (_, _, stack, _,_, _, _, _) = state
      state2 = applyToStats statIncSteps state

final ([], frame, stack, vstack, dump, heap, cstore, stats) = True
final state                                                 = False

applyToStats f (instr, frame, stack, vstack, dump, heap, cstore, stats)
  = (instr, frame, stack, vstack, dump, heap, cstore, f stats)

step ((Take t n:instr), fptr, stack, vstack, dump, heap, cstore, stats)
  | length stack >= n = (instr, fptr', (drop n stack), vstack, dump, heap', cstore, stats)
  | otherwise         = error "Too few args for Take"
    where
      (heap', fptr') = fAlloc heap frames
      frames = (take n stack) ++ (take (t - n) (repeat ([], FrameNull)))

step ((Move i a:instr), fptr, stack, vstack, dump, heap, cstore, stats)
  = (instr, fptr, stack, vstack, dump, heap', cstore, stats)
    where
      heap' = fUpdate heap fptr i (amToClosure a fptr heap cstore)

step ([Enter am], fptr, stack, vstack, dump, heap, cstore, stats)
  = (instr', fptr', stack, vstack, dump, heap, cstore, stats)
    where
      (instr', fptr') = amToClosure am fptr heap cstore

step ((Push am:instr), fptr, stack, vstack, dump, heap, cstore, stats)
  = (instr, fptr, stack', vstack, dump, heap, cstore, stats)
    where
      stack' = (amToClosure am fptr heap cstore) : stack

step ((PushV FramePtr:instr), (FrameInt n), stack, vstack, dump, heap, cstore, stats)
  = (instr, (FrameInt n), stack, n : vstack, dump, heap, cstore, stats)

step ((PushV (IntVConst n):instr), fptr, stack, vstack, dump, heap, cstore, stats)
  = (instr, fptr, stack, n : vstack, dump, heap, cstore, stats)

step ([Return], fptr, ((i', f'):stack), vstack, dump, heap, cstore, stats)
  = (i', f', stack, vstack, dump, heap, cstore, stats)

step ((Op Add:instr), fptr, stack, (n1:n2:vstack), dump, heap, cstore, stats)
  = (instr, fptr, stack, (n1 + n2):vstack, dump, heap, cstore, stats)

step ((Op Mult:instr), fptr, stack, (n1:n2:vstack), dump, heap, cstore, stats)
  = (instr, fptr, stack, (n1 * n2):vstack, dump, heap, cstore, stats)

step ((Op Sub:instr), fptr, stack, (n1:n2:vstack), dump, heap, cstore, stats)
  = (instr, fptr, stack, (n1 - n2):vstack, dump, heap, cstore, stats)

step ((Op Div:instr), fptr, stack, (n1:n2:vstack), dump, heap, cstore, stats)
  = (instr, fptr, stack, (n1 `div` n2):vstack, dump, heap, cstore, stats)

step ((Op Lt:instr), fptr, stack, (n1:n2:vstack), dump, heap, store, stats)
  = (instr, fptr, stack, isLt:vstack, dump, heap, store, stats)
    where isLt | (n2 < n1) = 1 | otherwise = 0

step ([Cond i1 i2], fptr, stack, 0:vstack, dump, heap, cstore, stats)
  = (i1, fptr, stack, vstack, dump, heap, cstore, stats)

step ([Cond i1 i2], fptr, stack, n:vstack, dump, heap, cstore, stats)
  = (i2, fptr, stack, vstack, dump, heap, cstore, stats)

step state = error ("No match found for state: " ++ iDisplay (showState state))

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

showDump _       = iStr "[dump]" `iAppend` iNewline

showValueStack vstack
  = iConcat [ iStr "Value stack: [",
              iIndent (iInterleave iNewline (map iNum vstack)),
              iStr "]", iNewline ]

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
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline
            , iStr "Max stack depth = ", iNum (maxStackDepth stats), iNewline
            , iStr "No of frames allocated = ", iNum (U.hSize heap), iNewline
            ]

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

showInstruction d (Take t n)  = iConcat [iStr "Take ", iNum t, iStr " ", iNum n]
showInstruction d (Enter x) = (iStr "Enter ") `iAppend` (showArg d x)
showInstruction d (Push x)  = (iStr "Push ")  `iAppend` (showArg d x)
showInstruction d (Return)  = (iStr "Return")
showInstruction d (Op op)   = (iStr "Op ") `iAppend` (iStr (show op))
showInstruction d (Move i a) = iConcat[ iStr "Move ", iNum i, iStr " ", (showArg d a) ]
showInstruction d (PushV FramePtr) = (iStr "PushV FramePtr")

showInstruction d (Cond i1 i2)
  = iConcat [ iStr "Cond [",
              iIndent (iInterleave iNewline [(showInstructions d i1), (showInstructions d i2)]),
              iStr "]", iNewline ]

showInstruction d i = error ("Unknown instruction " ++ show i)

showArg d (Arg m)      = (iStr "Arg ")   `iAppend` (iNum m)
showArg d (Code il)    = (iStr "Code ")  `iAppend` (showInstructions d il)
showArg d (Label s)    = (iStr "Label ") `iAppend` (iStr s)
showArg d (IntConst n) = (iStr "IntConst ") `iAppend` (iNum n)

nTerse = 3
