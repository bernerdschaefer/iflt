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

coreExprToANF' (ELet False letDefns body) defns
  = (ELet False letDefns' body', defns'')
    where
      (letDefns', defns') = coreDefnsToANF letDefns defns
      (body', defns'') = coreExprToANF' body defns'

coreExprToANF' (ELam vars body) defns
  = (ELam vars body', defns)
    where
      body' = coreExprToANF body

coreExprToANF' expr@(ECase e alters) defns
  = (ECase e' alters', defns')
    where
      (e', defns') = coreExprToANF' e defns
      alters' = map coreAltToANF alters

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

coreDefnsToANF :: [CoreDefn] -> [CoreDefn] -> ([CoreDefn], [CoreDefn])
coreDefnsToANF letDefns defns
  = coreDefnsToANF' letDefns ([], defns)

coreDefnsToANF' [] accum = accum
coreDefnsToANF' (letDefn:letDefns) (accumLetDefns, defns)
  = coreDefnsToANF' letDefns (letDefn' : accumLetDefns, defns')
    where
      (letDefn', defns') = coreDefnToANF letDefn defns

coreDefnToANF :: CoreDefn -> [CoreDefn] -> (CoreDefn, [CoreDefn])
coreDefnToANF (name, body) defns
  = ((name, body'), defns')
    where
      (body', defns') = coreExprToANF' body defns

coreAltToANF :: CoreAlt -> CoreAlt
coreAltToANF (tag, vars, e)
  = (tag, vars, e')
    where
      e' = coreExprToANF e

newId :: String -> [CoreDefn] -> Name
newId prefix defns = "**" ++ prefix ++ (show (length defns)) ++ "**"

example1
  = pprint . coreProgramToANF $ parse "main = f 1 (f 2 (f 3))"

example2
  = pprint . coreProgramToANF $ parse "main = let x = f 1 (f 2) in x"

example3
  = pprint . coreProgramToANF $ parse "main = case (f 1 (f 2)) of <1> -> g 1 (f 2)"

example4
  = pprint . coreProgramToANF $ parse "main = (\\ x . f 1 (f 2)) (f 2)"
