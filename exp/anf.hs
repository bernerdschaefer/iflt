type Id = String

data CoreExpr
  = ENum Int
  | EVar Id
  | EAp CoreExpr CoreExpr
  | ELet [Defn] CoreExpr
  deriving (Show)

type Defn = (Id, CoreExpr)

coreExprToANF :: CoreExpr -> CoreExpr
coreExprToANF expr
  = anfExpr
    where
      maybeLet e []    = e
      maybeLet e defns = ELet defns e

      (expr', defns) = coreExprToANF' expr []

      anfExpr = maybeLet expr' defns

coreExprToANF' :: CoreExpr -> [Defn] -> (CoreExpr, [Defn])
coreExprToANF' expr@(EAp _ _) defns = coreApToANF expr defns
coreExprToANF' e              defns = (e, defns)

coreApToANF (EAp f a) defns
  = (anfApToCore ap, defns')
    where
      (ap, defns') = coreApToANF' f a defns

coreApToANF' :: CoreExpr -> CoreExpr -> [Defn] -> (AnfAp, [Defn])
coreApToANF' f (ENum n) defns
  = (AnfAp f' (AnfNum n), defns')
    where
      (f', defns') = coreExprToANF' f defns
coreApToANF' f (EVar v) defns
  = (AnfAp f' (AnfVar v), defns')
    where
      (f', defns') = coreExprToANF' f defns
coreApToANF' f a defns
  = (AnfAp f1 a1, defns3)
    where
      id = newId defns
      a1 = (AnfVar id)
      defns1 = (id, a2) : defns
      (f1, defns2) = coreExprToANF' f defns1
      (a2, defns3) = coreExprToANF' a defns2

newId :: [Defn] -> Id
newId defns = "**x" ++ (show (length defns)) ++ "**"

data AnfAp = AnfAp CoreExpr AnfArg

anfApToCore :: AnfAp -> CoreExpr
anfApToCore (AnfAp f a)
  = (EAp f a')
    where a' = anfArgToCore a

data AnfArg
  = AnfNum Int
  | AnfVar Id

anfArgToCore :: AnfArg -> CoreExpr
anfArgToCore (AnfNum i) = ENum i
anfArgToCore (AnfVar v) = EVar v

-- coreExprToANF (EAp (EVar "f") (ENum 1))
-- coreExprToANF (EAp (EVar "f") (EAp (EVar "f") (ENum 2)))
-- coreExprToANF (EAp (EVar "f") (EAp (EAp (EVar "f") (ENum 2)) (EAp (EVar "f") (ENum 2))))
