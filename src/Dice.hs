module Dice where

import System.Random

data Dice
   = Die   Integer
   | Const Integer
   | Plus  Dice Dice
   | Scale Integer Dice
   deriving (Show)

instance Num Dice where
  fromInteger = Const
  (+) = Plus

(*.) :: Integer -> Dice -> Dice
(*.) = Scale
infixr 7 *.

d :: Integer -> Dice
d = Die

onDice :: DiceAction t -> Dice -> t
onDice with = whenDiceIs where
  whenDiceIs (Die number) = onDie with number
  whenDiceIs (Const number) = onConst with number
  whenDiceIs (Plus dice1 dice2) = onPlus with (whenDiceIs dice1) (whenDiceIs dice2)
  whenDiceIs (Scale factor dice) = onScale with factor (whenDiceIs dice)

data DiceAction t = DiceAction {
  onDie :: Integer -> t,
  onConst :: Integer -> t,
  onPlus :: t -> t -> t,
  onScale :: Integer -> t -> t
}

roll :: Dice -> IO Integer
roll = onDice DiceAction {
  onDie = \n -> randomRIO (1, n),
  onConst = \n -> return n,
  onPlus = \d1 -> \d2 -> do { x <- d1; y <- d2; return (x + y ) },
  onScale = \n -> \d -> (n *) <$> d
}

range :: Dice -> (Integer, Integer)
range = onDice DiceAction {
  onDie = \n -> (1, n),
  onConst = \n -> (n, n),
  onPlus = \(min1, max1) -> \(min2, max2) -> (min1 + min2, max1 + max2),
  onScale = \n -> \(mn, mx) -> (n * mn, n * mx)
}

expected :: Dice -> Double
expected = onDice DiceAction {
  onDie = \n -> fromIntegral (1 + n) / 2,
  onConst = fromIntegral,
  onPlus = (+),
  onScale = (*) . fromIntegral
}
