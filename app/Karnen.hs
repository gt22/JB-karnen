-- Based on https://gist.github.com/msullivan/4223fd47991acbe045ec#file-microkanren-hs
module Karnen where

import Control.Applicative
import Control.Monad

type Var = Integer

type Subst = [(Var, Term)]

type State = (Subst, Integer)

type Program = State -> KList State

data Term
  = RelVar Var
  | BTrue
  | BFalse
  | BVar Term -- Var Nat
  | Term :/\ Term -- Conj BTerm BTerm
  | Term :\/ Term -- Disj BTerm BTerm
  | BNeg Term -- Neg BTerm
  | NZero
  | NSuc Term -- Suc Nat
  | LNil 
  | LCons Term Term -- Cons Bool List
  deriving (Eq, Show)

-- Apply a substitution to the top level of a term
walk :: Term -> [(Var, Term)] -> Term
walk (RelVar v) s = case lookup v s of
  Nothing -> RelVar v
  Just us -> walk us s
walk u _ = u

extS :: a -> b -> [(a, b)] -> [(a, b)]
extS x v s = (x, v) : s

-- Try to unify two terms under a substitution;
-- return an extended subst if it succeeds
unify :: Term -> Term -> Subst -> Maybe Subst
unify u' v' s = un (walk u' s) (walk v' s)
  where
    un (RelVar x1) (RelVar x2) | x1 == x2 = return s
    un (RelVar x1) v = return $ extS x1 v s
    un u (RelVar x2) = return $ extS x2 u s
    un BTrue BTrue = return s
    un BFalse BFalse = return s
    un (BVar a) (BVar b) = unify a b s
    un (a1 :/\ b1) (a2 :/\ b2) = do
      s' <- unify a1 a2 s
      unify b1 b2 s'
    un (a1 :\/ b1) (a2 :\/ b2) = do
      s' <- unify a1 a2 s
      unify b1 b2 s'
    un (BNeg a) (BNeg b) = unify a b s
    un NZero NZero = return s
    un (NSuc a) (NSuc b) = unify a b s
    un LNil LNil = return s
    un (LCons h1 t1) (LCons h2 t2) = do
      s' <- unify h1 h2 s
      unify t1 t2 s'
    un _ _ = mzero

-- MicroKanren program formers
zzz :: Program -> Program
zzz g = \sc -> delay (g sc)

equiv :: Term -> Term -> Program
equiv u v = \(s, c) -> case unify u v s of
  Nothing -> mzero
  Just s' -> return (s', c)

callFresh :: (Term -> Program) -> Program
callFresh f = \(s, c) -> f (RelVar c) (s, c + 1)

disj :: Program -> Program -> Program
disj g1 g2 = \sc -> mplus (g1 sc) (g2 sc)

conj :: Program -> Program -> Program
conj g1 g2 = \sc -> g1 sc >>= g2

-- I had originally thought that since Haskell was lazy, we didn't
-- need to worry about any of the inverse-eta-delay stuff that the
-- Scheme version does, but that isn't right. We still need some way
-- to force switching when we recur.
-- It is not very burdensome for us, though; we don't actually need
-- to eta-expand, just need to add some sort of marker.
data KList a = Nil | Cons a (KList a) | Delay (KList a) deriving (Show)

delay = Delay

-- Hm. Is there any reason to preserve the delays?

instance Functor KList where
  fmap = liftM

instance Applicative KList where
  pure = return
  (<*>) = ap

instance Monad KList where
  return x = Cons x Nil
  Nil >>= _ = mzero
  x `Cons` xs >>= f = f x `mplus` (xs >>= f)
  Delay xs >>= f = Delay (xs >>= f)

instance Alternative KList where
  empty = mzero
  (<|>) = mplus

instance MonadPlus KList where
  mzero = Nil
  Nil `mplus` xs = xs
  (x `Cons` xs) `mplus` ys = x `Cons` (ys `mplus` xs) -- swapped per sect. 6
  Delay xs `mplus` ys = Delay (ys `mplus` xs)

klistToList :: KList a -> [a]
klistToList Nil = []
klistToList (x `Cons` xs) = x : klistToList xs
klistToList (Delay xs) = klistToList xs

run :: Program -> [State]
run p = klistToList (p ([], 0))


disj' :: [Program] -> Program
disj' = foldr1 disj

conj' :: [Program] -> Program
conj' = foldr1 conj

fresh :: (Term -> Program) -> Program
fresh = callFresh

fresh2 :: (Term -> Term -> Program) -> Program
fresh2 f = fresh $ \x -> fresh $ \y -> f x y

fresh3 :: (Term -> Term -> Term -> Program) -> Program
fresh3 f = fresh $ \x -> fresh $ \y -> fresh $ \z -> f x y z

fresh4 :: (Term -> Term -> Term -> Term -> Program) -> Program
fresh4 f = fresh $ \x -> fresh $ \y -> fresh $ \z -> fresh $ \w -> f x y z w

infix 3 ===, ===?

(===) :: Term -> Term -> Program
(===) = equiv

(===?) :: Term -> Maybe Term -> Program
_ ===? Nothing = pure
a ===? (Just b) = a === b

desub :: Subst -> Term -> Term
desub s (RelVar v) = case lookup v s of
  Nothing -> RelVar v
  Just t -> desub s t
desub s (BVar t) = BVar $ desub s t
desub s (a :/\ b) = desub s a :/\ desub s b
desub s (a :\/ b) = desub s a :\/ desub s b
desub s (BNeg t) = BNeg $ desub s t
desub s (NSuc t) = NSuc $ desub s t
desub s (LCons a b) = LCons (desub s a) (desub s b)
desub _ t = t