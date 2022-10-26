module Functional.Evals where

import Control.Monad (mzero)
import Functional.Connectors
import Functional.Elems
import Functional.Generators
import Karnen
import Utils
import Prelude hiding (elem, not)

-- evalo in in in
evalStTR :: [Bool] -> Term -> Bool -> [()]
evalStTR _ BTrue True = return ()
evalStTR _ BFalse False = return ()
evalStTR st (BVar i) res = elemIStR i st res
evalStTR st (BNeg a) res = do
  x <- negR res
  evalStTR st a x
evalStTR st (a :/\ b) res = do
  (x, y) <- andR res
  evalStTR st a x
  evalStTR st b y
evalStTR st (a :\/ b) res = do
  (x, y) <- orR res
  evalStTR st a x
  evalStTR st b y
evalStTR _ _ _ = mzero

-- evalo in in out
evalStT :: [Bool] -> Term -> [Bool]
evalStT _ BTrue = return True
evalStT _ BFalse = return False
evalStT st (BVar i) = elemISt i st
evalStT st (BNeg t) = do
  x <- evalStT st t
  negX x
evalStT st (a :/\ b) = do
  x <- evalStT st a
  y <- evalStT st b
  andXY x y
evalStT st (a :\/ b) = do
  x <- evalStT st a
  y <- evalStT st b
  orXY x y
evalStT _ _ = mzero

-- evalo in out in
evalStR :: [Bool] -> Bool -> [Term]
evalStR st r = mix [btrue r, bfalse r, vars, conjs, disjs, negs]
  where
    btrue True = return true
    btrue _ = mzero

    bfalse False = return false
    bfalse _ = mzero

    vars = var <$> elemStR st r

    conjs = do
      (x, y) <- andR r
      a <- evalStR st x
      b <- evalStR st y
      return $ a :/\ b

    disjs = do
      (x, y) <- orR r
      a <- evalStR st x
      b <- evalStR st y
      return $ a :\/ b

    negs = do
      x <- negR r
      a <- evalStR st x
      return $ not a

-- evalo out in in
evalTR :: Int -> Term -> Bool -> [[Bool]]
evalTR maxListLength BTrue True = genState maxListLength
evalTR maxListLength BFalse False = genState maxListLength
evalTR maxListLength (BVar i) r = elemIR maxListLength i r
evalTR maxListLength (a :/\ b) r = do
  (x, y) <- andR r
  st <- evalTR maxListLength a x
  evalStTR st b y
  return st
evalTR maxListLength (a :\/ b) r = do
  (x, y) <- orR r
  st <- evalTR maxListLength a x
  evalStTR st b y
  return st
evalTR maxListLength (BNeg a) r = do
  x <- negR r
  evalTR maxListLength a x
evalTR _ _ _ = mzero

-- evalo in out out
evalSt :: [Bool] -> [(Term, Bool)]
evalSt st = mix [btrue, bfalse, vars, conjs, disjs, negs]
  where
    btrue = return (true, True)
    bfalse = return (false, False)
    vars = do
      (t, r) <- elemSt st
      return (var t, r)

    conjs = do
      (a, x) <- evalSt st
      (b, y) <- evalSt st
      r <- andXY x y
      return (a :/\ b, r)

    disjs = do
      (a, x) <- evalSt st
      (b, y) <- evalSt st
      r <- orXY x y
      return (a :\/ b, r)

    negs = do
      (a, x) <- evalSt st
      r <- negX x
      return (not a, r)

-- evalo out in out
evalT :: Int -> Term -> [([Bool], Bool)]
evalT maxListLength BTrue = do
  st <- genState maxListLength
  return (st, True)
evalT maxListLength BFalse = do
  st <- genState maxListLength
  return (st, False)
evalT maxListLength (BVar i) = do
  (st, res) <- elemI maxListLength i
  return (st, res)
evalT maxListLength (a :/\ b) = do
  (st, x) <- evalT maxListLength a
  y <- evalStT st b
  res <- andXY x y
  return (st, res)
evalT maxListLength (a :\/ b) = do
  (st, x) <- evalT maxListLength a
  y <- evalStT st b
  res <- orXY x y
  return (st, res)
evalT maxListLength (BNeg a) = do
  (st, x) <- evalT maxListLength a
  res <- negX x
  return (st, res)
evalT _ _ = mzero

-- evalo out out in
evalR :: Int -> Bool -> [([Bool], Term)]
evalR maxListLength r = mix [btrue r, bfalse r, vars, conjs, disjs, negs]
  where
    btrue True = do
      st <- genState maxListLength
      return (st, true)
    btrue _ = mzero

    bfalse False = do
      st <- genState maxListLength
      return (st, false)
    bfalse _ = mzero

    vars = do
      (i, st) <- elemR maxListLength r
      return (st, var i)

    conjs = do
      (x, y) <- andR r
      (st, a) <- evalR maxListLength x
      b <- evalStR st y
      return (st, a :/\ b)

    disjs = do
      (x, y) <- orR r
      (st, a) <- evalR maxListLength x
      b <- evalStR st y
      return (st, a :\/ b)

    negs = do
      x <- negR r
      (st, a) <- evalR maxListLength x
      return (st, not a)

-- evalo out out out
eval :: Int -> [([Bool], Term, Bool)]
eval maxListLength = mix [btrue, bfalse, vars, conjs, disjs, negs]
  where
    btrue = do
      st <- genState maxListLength
      return (st, true, True)

    bfalse = do
      st <- genState maxListLength
      return (st, false, False)

    vars = do
      (i, st, r) <- elem maxListLength
      return (st, var i, r)

    conjs = do
      (st, a, x) <- eval maxListLength
      (b, y) <- evalSt st
      r <- andXY x y
      return (st, a :/\ b, r)

    disjs = do
      (st, a, x) <- eval maxListLength
      (b, y) <- evalSt st
      r <- andXY x y
      return (st, a :\/ b, r)

    negs = do
      (st, a, x) <- eval maxListLength
      r <- negX x
      return (st, not a, r)

evaluteTerm :: [Bool] -> Term -> Bool
evaluteTerm st t = head $ evalStT st t

satisfyTerm :: Term -> [[Bool]]
satisfyTerm t = evalTR (1 + maxVar t) t True

findTerms :: Int -> Bool -> [String]
findTerms maxListLength r = map (showGenerated . \(st, t) -> (st, t, r)) $ evalR maxListLength r

generateTerms :: Int -> [String]
generateTerms = map showGenerated . eval
