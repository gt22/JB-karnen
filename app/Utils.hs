module Utils where

import Karnen
  
true :: Term
true = BTrue

false :: Term
false = BFalse

not :: Term -> Term
not = BNeg

var :: Term -> Term
var = BVar

var' :: Integer -> Term
var' = var . toNumberTerm
  
fromListTerm :: Term -> [Bool]
fromListTerm LNil = []
fromListTerm (RelVar _) = []
fromListTerm (LCons b t) = fromBoolTerm b : fromListTerm t
fromListTerm t = error $ "Not a list term " ++ show t

toListTerm :: [Bool] -> Term
toListTerm = foldr (LCons . toBoolTerm) LNil

fromBoolTerm :: Term -> Bool
fromBoolTerm BTrue = True
fromBoolTerm BFalse = False
fromBoolTerm t = error $ "Not a bool term " ++ show t

toBoolTerm :: Bool -> Term
toBoolTerm True = BTrue
toBoolTerm False = BFalse

fromNumberTerm :: Term -> Int
fromNumberTerm NZero = 0
fromNumberTerm (NSuc x) = 1 + fromNumberTerm x
fromNumberTerm t = error $ "Not a number term " ++ show t

toNumberTerm :: (Num t, Ord t) => t -> Term
toNumberTerm 0 = NZero
toNumberTerm n | n > 0 = NSuc $ toNumberTerm $ n - 1
toNumberTerm _ = error "Negative numbers are not supported"

showFormula :: Term -> String
showFormula BTrue = "true"
showFormula BFalse = "false"
showFormula (a :/\ b) = "(" ++ showFormula a ++ " /\\ " ++ showFormula b ++ ")"
showFormula (a :\/ b) = "(" ++ showFormula a ++ " \\/ " ++ showFormula b ++ ")"
showFormula (BNeg a) = "!" ++ showFormula a
showFormula (BVar n) = show $ fromNumberTerm n
showFormula t = error $ "Not a formula term " ++ show t

showGenerated :: ([Bool], Term, Bool) -> String
showGenerated (s, f, r) = showFormula f ++ " :: " ++ show s ++ " = " ++ show r

mix :: [[a]] -> [a]
mix [] = []
mix xs = let (h, t) = multisplit xs in h ++ mix t

multisplit :: [[a]] -> ([a], [[a]])
multisplit [] = ([], [])
multisplit ([] : xs) = multisplit xs
multisplit ((a : as) : xs) = let (m, ms) = multisplit xs in (a : m, as : ms)

maxVar :: Term -> Int
maxVar (BVar i) = fromNumberTerm i
maxVar (a :/\ b) = maxVar a `max` maxVar b
maxVar (a :\/ b) = maxVar a `max` maxVar b
maxVar (BNeg a) = maxVar a
maxVar _ = 0