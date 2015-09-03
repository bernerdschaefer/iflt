module STG where
import Language
import Transform
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
             | PackApp Pack Atoms
             | ConApp Constr Atoms
             | PrimApp PrimOp Atoms
             | Literal Int
             deriving (Show)

type Vars = [Var]
type Var = Name

type Literal = Int

data PrimOp = Add | Sub deriving (Show)

type Constr = Name
data Pack   = Pack Int Int deriving (Show)

type Alts = [Alt]
data Alt  = AlgAlt Constr Vars StgExpr
          | PackAlt Int Vars StgExpr
          | PrimAlt Literal StgExpr
          | NormAlt Var StgExpr
          | DefaultAlt StgExpr
          deriving (Show)

type Atoms = [Atom]
data Atom  = VarArg Var
           | LitArg Literal
           deriving (Show)

type Lambda = (Vars, UpdateFlag, Vars, StgExpr)

data UpdateFlag = Updateable | NonUpdateable deriving (Show)

--
-- Printing
--

pprStgProgram :: StgProgram -> Iseq
pprStgProgram program = pprBinds program

pprBinds binds
  = iInterleave (iConcat [iStr ";", iNewline]) (map pprBind binds)

pprBind :: Bind -> Iseq
pprBind (name, lf)
  = iConcat [ iStr name
            , iStr " = "
            , pprLambdaForm lf ]

pprLambdaForm :: Lambda -> Iseq
pprLambdaForm (free, u, vars, body)
  = iConcat [ pprVars free
            , iStr " "
            , pprUpdateFlag u
            , iStr " "
            , pprVars vars
            , iStr " ->"
            , iNewline
            , iStr "  "
            , iIndent (pprStgExpr body) ]

pprStgExpr (Literal i) = pprLiteral i

pprStgExpr (Let isrec binds body)
  = iConcat [ iStr keyword
            , iNewline
            , iStr "  "
            , iIndent (pprBinds binds)
            , iNewline
            , iStr "in "
            , pprStgExpr body ]
    where
      keyword | isrec == True = "letrec"
              | otherwise     = "let"

pprStgExpr (Case expr alts)
  = iConcat [ iStr "case "
            , pprStgExpr expr
            , iStr " of"
            , iNewline
            , iStr "  "
            , iIndent (pprStgAlts alts) ]

pprStgExpr (App f args)
  = iConcat [ iStr f
            , iStr " "
            , pprAtoms args ]

pprStgExpr (ConApp constr args)
  = iConcat [ iStr constr
            , iStr " "
            , pprAtoms args ]

pprStgExpr (PackApp pack args)
  = iConcat [ pprPack pack
            , iStr " "
            , pprAtoms args ]

pprPack (Pack tag arity)
  = iConcat [ iStr "Pack{"
            , iNum tag
            , iStr ","
            , iNum arity
            , iStr "}" ]

pprUpdateFlag Updateable    = iStr "u"
pprUpdateFlag NonUpdateable = iStr "n"

pprStgAlts alts = iInterleave (iStr ";" `iAppend` iNewline) (map pprAlt alts)

pprAlt (AlgAlt constr vars expr)
  = iConcat [ iStr constr
            , iStr " "
            , pprVars vars
            , iStr " -> "
            , pprStgExpr expr ]

pprAlt (PackAlt tag vars expr)
  = iConcat [ iStr "<"
            , iNum tag
            , iStr "> "
            , pprVars vars
            , iStr " -> "
            , pprStgExpr expr ]

pprAtoms args
  = iConcat [ iStr "{"
            , iInterleave (iStr " ") (map pprAtom args)
            , iStr "}" ]

pprAtom (VarArg v) = pprVar v
pprAtom (LitArg i) = pprLiteral i

pprLiteral i = (iNum i) `iAppend` (iStr "#")

pprVars vars
  = iConcat [ iStr "{"
            , iInterleave (iStr ",") (map pprVar vars)
            , iStr "}" ]

pprVar var = (iStr var)

--
-- Transformation from Core to STG
--

transformCoreProgram :: CoreProgram -> StgProgram
transformCoreProgram program
  = map transformCoreScDefn program'
    where program' = coreProgramToANF program

transformCoreScDefn :: CoreScDefn -> Bind
transformCoreScDefn (name, args, body)
  = (name, ([], NonUpdateable, args, transformCoreExpr body))

transformCoreExpr expr@(EAp _ _)
  = transformCoreAp f args
    where (f, args) = flattenAp expr

transformCoreExpr (ECase e alts)
  = (Case (transformCoreExpr e) (transformCoreAlts alts))

transformCoreExpr var@(EVar v)
  = transformCoreAp var []

transformCoreExpr (ENum n) = (Literal n)

transformCoreExpr (EConstr tag arity@0)
  = PackApp (Pack tag arity) []

transformCoreExpr (ELet isRec defns body)
  = (Let isRec defns' (transformCoreExpr body))
    where
      defns' = map transformCoreDefn defns
      transformCoreDefn (name, e)
        = (name, ([], NonUpdateable, [], transformCoreExpr e))

transformCoreExpr e = error ("unknown expression " ++ (show e))

transformCoreAp :: CoreExpr -> [CoreExpr] -> StgExpr

transformCoreAp (EVar v) args = App v (atoms args)

transformCoreAp constr@(EConstr tag arity) args
  = PackApp (Pack tag arity) (atoms args)

atoms args = map atom args
  where
    atom (EVar v) = VarArg v
    atom (ENum n) = LitArg n
    atom e        = error ("not an atom: " ++ (show e))

flattenAp :: CoreExpr -> (CoreExpr, [CoreExpr])
flattenAp expr
  = flatten expr []
    where
      flatten constr@(EConstr _ _) args = (constr, args)
      flatten var@(EVar _)         args = (var, args)
      flatten (EAp f arg)   args = flatten f (arg:args)
      flatten expr          args = error ("unknown expr " ++ (show expr))

transformCoreAlts :: [CoreAlt] -> Alts
transformCoreAlts coreAlts
  = alts
    where
      alts = map transformCoreAlt coreAlts

transformCoreAlt :: CoreAlt -> Alt
transformCoreAlt (tag, vars, body)
  = PackAlt tag vars (transformCoreExpr body)

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
                       , body :: StgExpr
                       , varValues :: [Int]
                       }

type GlobalEnv = [(Name, Addr)]
type LocalEnv = [(Name, Value)]

type Stack = [Value]

type Heap = U.Heap Closure

type Addr = U.Addr

type Code = [Instruction]

data Value = AddrValue Addr
           | IntConst Int
           deriving (Show)

data Instruction = Eval StgExpr LocalEnv
                 | Enter Addr
                 | ReturnCon Int [Value]
                 | ReturnInt Int
                 deriving (Show)

vals :: LocalEnv -> GlobalEnv -> Atoms -> [Value]
vals local global [] = []
vals local global (x:xs) = (val local global x) : (vals local global xs)

val :: LocalEnv -> GlobalEnv -> Atom -> Value
val local global (LitArg n) = IntConst n
val local global (VarArg v)
  = U.aLookup local v lookupGlobal
    where
      lookupGlobal = AddrValue $ U.aLookup global v (error ("unknown variable " ++ v))

initialState
  = State { code = [Eval (App "main" []) []]
          , args = []
          , rets = []
          , upds = []
          , heap = U.hInitial
          , env  = []
          }

step state@State{code = [Eval (App f xs) []]}
  = state { code = [Enter addr]
          , args = (vals [] (env state) xs) ++ (args state)
          }
  where
    addr = U.aLookup (env state) f (error "not in environment")

step state@State{code = [Enter addr]}
  = state { code = [Eval e localEnv], args = args' }
    where
      closure = U.hLookup (heap state) addr
      e = (body closure)
      args' = (args state)
      localEnv = []

step State{code = code} = (error ("unknown expression " ++ (show code)))

--
-- Compilation
--

compileStgProgram :: StgProgram -> State -> State

compileStgProgram [] state = state

compileStgProgram (bind:binds) state
  = compileStgProgram binds state'
    where
      state' = state { heap = heap', env = env' }
      (name, (_, _, _, body)) = bind
      closure = Closure { updateable = False, body = body }
      (heap', addr) = U.hAlloc (heap state) closure
      env' = (name, addr) : (env state)
