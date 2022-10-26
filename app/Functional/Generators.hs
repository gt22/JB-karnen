module Functional.Generators where
genBool :: [Bool]
genBool = [False, True]

genState :: Int -> [[Bool]]
genState n = concatMap genStateFixed [0 .. n]
  where
    genStateFixed 0 = return []
    genStateFixed k = do
      h <- genBool
      t <- genStateFixed (k - 1)
      return $ h : t