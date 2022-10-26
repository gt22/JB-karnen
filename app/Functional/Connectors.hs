module Functional.Connectors where

-- nego out in
negR :: Bool -> [Bool]
negR True = [False]
negR False = [True]

-- ando out out in
andR :: Bool -> [(Bool, Bool)]
andR True = [(True, True)]
andR False = [(True, False), (False, True), (False, False)]

-- oro out out in
orR :: Bool -> [(Bool, Bool)]
orR True = [(True, False), (False, True), (True, True)]
orR False = [(False, False)]

-- nego in out
negX :: Bool -> [Bool]
negX True = [False]
negX False = [True]

-- ando in in out
andXY :: Bool -> Bool -> [Bool]
andXY True True = [True]
andXY True False = [False]
andXY False True = [False]
andXY False False = [False]

-- oro in in out
orXY :: Bool -> Bool -> [Bool]
orXY True True = [True]
orXY True False = [True]
orXY False True = [True]
orXY False False = [False]