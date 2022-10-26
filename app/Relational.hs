module Relational where

import Data.Maybe (fromJust)
import Karnen
import Utils
import Prelude hiding (not)

isBool :: Term -> Program
isBool x = disj' [x === true, x === false]

elemo :: Term -> Term -> Term -> Program
elemo i state res =
  disj'
    [ fresh $ \t -> conj' [i === NZero, state === LCons res t],
      fresh3 $ \i' h t -> conj' [i === NSuc i', state === LCons h t, elemo i' t res, isBool h]
    ]

nego :: Term -> Term -> Program
nego x res =
  disj'
    [ conj' [x === true, res === false],
      conj' [x === false, res === true]
    ]

oro :: Term -> Term -> Term -> Program
oro a b res =
  disj'
    [ conj' [a === true, b === true, res === true],
      conj' [a === true, b === false, res === true],
      conj' [a === false, b === true, res === true],
      conj' [a === false, b === false, res === false]
    ]

ando :: Term -> Term -> Term -> Program
ando a b res =
  disj'
    [ conj' [a === true, b === true, res === true],
      conj' [a === true, b === false, res === false],
      conj' [a === false, b === true, res === false],
      conj' [a === false, b === false, res === false]
    ]

evalo :: Term -> Term -> Term -> Program
evalo state term res =
  disj'
    [ conj' [term === true, res === true],
      conj' [term === false, res === false],
      fresh $ \z -> conj' [term === var z, elemo z state res],
      fresh4 $ \a b x y -> conj' [term === a :/\ b, ando x y res, evalo state a x, evalo state b y],
      fresh4 $ \a b x y -> conj' [term === a :\/ b, oro x y res, evalo state a x, evalo state b y],
      fresh2 $ \a x -> conj' [term === not a, nego x res, evalo state a x]
    ]

-- Ограничение на длину списка чтобы было больше похоже на конвертированную версию, можно легко убрать
setMaxListLength :: Int -> Term -> Program
setMaxListLength 0 state = state === LNil
setMaxListLength maxListLength state =
  disj'
    [ state === LNil,
      fresh2 $ \h t -> conj' [isBool h, state === LCons h t, setMaxListLength (maxListLength - 1) t]
    ]

limitState :: Either [Bool] Int -> Term -> Program
limitState (Left st) state = state === toListTerm st
limitState (Right maxListLength) state = setMaxListLength maxListLength state

-- Создание свежих переменных и унификация делается для того, чтобы можно было одинаковыми способоами вытаскивать реузльтат, можно вместо этого прямо передавать нужные термы
runEval :: Either [Bool] Int -> Maybe Term -> Maybe Bool -> [State]
runEval st t e = run $ fresh3 $ \state term res -> conj' [limitState st state, term ===? t, res ===? toBoolTerm <$> e, evalo state term res]

generateTerms :: Int -> [String]
generateTerms maxListLength = map (showGenerated . extract) $ runEval (Right maxListLength) Nothing Nothing

findTerms :: Int -> Bool -> [String]
findTerms maxListLength r = map (showGenerated . extract) $ runEval (Right maxListLength) Nothing (Just r)

satisfyTerm :: Term -> [[Bool]]
satisfyTerm t = map extractState $ runEval (Right $ 1 + maxVar t) (Just t) (Just True)

evaluteTerm :: [Bool] -> Term -> Bool
evaluteTerm st t = head $ map extractValue $ runEval (Left st) (Just t) Nothing

------ Технические детали

extractState :: (Subst, b) -> [Bool]
extractState (sub, _) = fromListTerm $ maybe LNil (desub sub) $ lookup 0 sub

extractFormula :: (Subst, b) -> Term
extractFormula (sub, _) = desub sub $ fromJust $ lookup 1 sub

extractValue :: (Subst, b) -> Bool
extractValue (sub, _) = fromBoolTerm $ desub sub $ fromJust $ lookup 2 sub

extract :: (Subst, b) -> ([Bool], Term, Bool)
extract s = (extractState s, extractFormula s, extractValue s)
