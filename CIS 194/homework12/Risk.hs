{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import           Control.Monad.Random
import           Data.List
import           Data.Ord

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
        random = first DV . randomR (1, 6)
        randomR (low, hi) =
                first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

maxAttackers :: Battlefield -> Army
maxAttackers bf = max 0 (min 3 (attackers bf - 1))

maxDefenders :: Battlefield -> Army
maxDefenders bf = max 0 (min 2 (defenders bf - 1))

roll :: Army -> Rand StdGen [DieValue]
roll n = replicateM n die

fight :: Army -> Army -> Rand StdGen [(DieValue, DieValue)]
fight attackers defenders = zip <$> descending (roll attackers) <*> descending
        (roll defenders)
        where descending = fmap (sortOn Down)

modifyBattlefield :: (Army, Army) -> Battlefield -> Battlefield
modifyBattlefield (attackWins, defenseWins) bf =
        Battlefield (attackers bf + attackWins) (defenders bf + defenseWins)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
        pairings <- fight (maxAttackers bf) (maxDefenders bf)
        let wins = foldr
                    (\(attack, defense) (ta, td) -> if attack > defense
                            then (ta + 1, td)
                            else (ta, td + 1)
                    )
                    (0, 0)
                    pairings
        return $ modifyBattlefield wins bf


