module Functional.Elems where
import Prelude hiding (elem)
import Control.Monad (mzero, guard)
import Utils
import Karnen
import Functional.Generators

-- elemo in in in
elemIStR :: Term -> [Bool] -> Bool -> [()]
elemIStR NZero (r : _) r' | r == r' = return ()
elemIStR (NSuc i) (_ : t) r = elemIStR i t r
elemIStR _ _ _ = mzero

-- elemo in in out
elemISt :: Term -> [Bool] -> [Bool]
elemISt NZero (r : _) = return r
elemISt (NSuc i) (_ : t) = elemISt i t
elemISt _ _ = mzero

-- elemo in out in
elemIR :: Int -> Term -> Bool -> [[Bool]]
elemIR 0 _ _ = mzero
elemIR maxListLength NZero r = do
  t <- genState (maxListLength - 1)
  return $ r : t
elemIR maxListLength (NSuc i) r = do
  h <- genBool
  t <- elemIR (maxListLength - 1) i r
  return $ h : t
elemIR _ _ _ = mzero

-- elemo out in in
elemStR :: [Bool] -> Bool -> [Term]
elemStR (h : t) r = mix [zero, suc]
  where
    zero = do
      guard $ h == r
      return NZero

    suc = do
      i <- elemStR t r
      return $ NSuc i
elemStR _ _ = mzero

-- elemo in out out
elemI :: Int -> Term -> [([Bool], Bool)]
elemI 0 _ = []
elemI maxListLength NZero = do
  h <- genBool
  t <- genState (maxListLength - 1)
  return (h : t, h)
elemI maxListLength (NSuc i) = do
  h <- genBool
  (t, res) <- elemI (maxListLength - 1) i
  return (h : t, res)
elemI _ _ = mzero

-- elemo out in out
elemSt :: [Bool] -> [(Term, Bool)]
elemSt (h : t) = mix [zeros, sucs]
  where
    zeros = return (NZero, h)
    sucs = do
      (i, v) <- elemSt t
      return (NSuc i, v)
elemSt _ = mzero

-- elemo out out in
elemR :: Int -> Bool -> [(Term, [Bool])]
elemR 0 _ = mzero
elemR maxListLength r = mix [zeros, sucs]
  where
    zeros = do
      t <- genState (maxListLength - 1)
      return (NZero, r : t)
    sucs = do
      h <- genBool
      (i, t) <- elemR (maxListLength - 1) r
      return (NSuc i, h : t)

-- elemo out out out
elem :: Int -> [(Term, [Bool], Bool)]
elem 0 = mzero
elem maxListLength = mix [zeros, sucs]
  where
    zeros = do
      h <- genBool
      t <- genState (maxListLength - 1)
      return (NZero, h : t, h)

    sucs = do
      h <- genBool
      (i, t, res) <- elem (maxListLength - 1)
      return (NSuc i, h : t, res)