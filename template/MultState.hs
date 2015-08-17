import Language

type MultState = (Int, Int, Int, Int) -- (n, m, d, t)

evalMult :: MultState -> [MultState]
evalMult state
  | multFinal state = [state]
  | otherwise       = state : evalMult (stepMult state)

stepMult (n, m, d, t)
  | d > 0  = (n, m,   d-1, t+1)
  | d == 0 = (n, m-1, n,   t)

multFinal :: MultState -> Bool
multFinal (_, m, d, _) = m == 0 && d == 0
