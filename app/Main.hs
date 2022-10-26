module Main where

import Prelude hiding (not)
import Karnen (Term((:/\), (:\/)))
import Utils
import qualified Relational as R
import qualified Functional.Evals as F

main :: IO ()
main =
  do
    print "R.generateTerms"
    putStrLn $ unlines $ take 20 (R.generateTerms 2)
    print "F.generateTerms"
    putStrLn $ unlines $ take 20 (F.generateTerms 2)
    print "R.findTerms"
    putStrLn $ unlines $ take 20 (R.findTerms 2 True)
    print "F.findTerms"
    putStrLn $ unlines $ take 20 (F.findTerms 2 True)
    print "R.satisfyTerm"
    print $ R.satisfyTerm $ var' 0 :/\ var' 1
    print "F.satisfyTerm"
    print $ F.satisfyTerm $ var' 0 :/\ var' 1
    print "R.evaluateTerm"
    print $ R.evaluteTerm [False] (not $ var' 0 :\/ true)
    print "F.evaluateTerm"
    print $ F.evaluteTerm [False] (not $ var' 0 :\/ true)
