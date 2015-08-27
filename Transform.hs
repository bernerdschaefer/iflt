module Transform where
import Language

--
-- Conversion to A-Normal Form
--

coreProgramToANF :: CoreProgram -> CoreProgram
coreProgramToANF program
  = map coreScDefnToANF program

coreScDefnToANF (name, args, body)
  = (name, args, body')
    where body' = coreExprToANF body

coreExprToANF :: CoreExpr -> CoreExpr
coreExprToANF expr
  = anfExpr
    where
      maybeLet e []    = e
      maybeLet e defns = ELet nonRecursive defns e

      (expr', defns) = coreExprToANF' expr []

      anfExpr = maybeLet expr' defns

coreExprToANF' :: CoreExpr -> [CoreDefn] -> (CoreExpr, [CoreDefn])

coreExprToANF' expr@(EAp f a) defns
  = coreApToANF f a defns

coreExprToANF' e defns = (e, defns)

coreApToANF :: CoreExpr -> CoreExpr -> [CoreDefn] -> (CoreExpr, [CoreDefn])

coreApToANF f (ENum n) defns
  = (EAp f' (ENum n), defns')
    where (f', defns') = coreExprToANF' f defns

coreApToANF f (EVar v) defns
  = (EAp f' (EVar v), defns')
    where (f', defns') = coreExprToANF' f defns

coreApToANF f a defns
  = (EAp f' (EVar id), defns3)
    where
      id = newId "anf" defns
      defns1 = (id, a') : defns
      (f', defns2) = coreExprToANF' f defns1
      (a', defns3) = coreExprToANF' a defns2

newId :: String -> [CoreDefn] -> Name
newId prefix defns = "**" ++ prefix ++ (show (length defns)) ++ "**"
