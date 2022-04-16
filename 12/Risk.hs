{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Monad.Random
import           Data.Bifunctor
import           Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = gemLeave bf af . battleOutcome af <$> dice (at + de)
  where af@(at, de) = getTroops bf

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

getTroops :: Battlefield -> (Army, Army)
getTroops (Battlefield at de) = (atT, deT)
  where
    atT = min 3 (at - 1)
    deT = min 2 (de - 1)

battleOutcome :: (Army, Army) -> [DieValue] -> (Army, Army)
battleOutcome t@(at, de) ds = compareDice sorted t
  where sorted = fmapPair (reverse . sort) (splitAt at ds)

fmapPair :: (a -> b) -> (a, a) -> (b, b)
fmapPair f (a, b) = (f a, f b)

compareDice :: ([DieValue], [DieValue]) -> (Army, Army) -> (Army, Army)
compareDice ([], _) t = t
compareDice (_, []) t = t
compareDice (a:as, d:ds) (aL, dL) = if a > d then compareDice (as, ds) (aL, dL-1) else compareDice (as, ds) (aL-1, dL)

gemLeave :: Battlefield -> (Army, Army) -> (Army, Army) -> Battlefield
gemLeave (Battlefield a1 d1) (at, dt) (al, dl) = Battlefield (a1 - at + al) (d1 - dt + dl)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf < 2 || defenders bf <= 0 = return bf
  | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = (/ 1000) . successCount <$> replicateM 1000 (invade bf)
  where
    successCount = fromIntegral . length . filter ((== 0) . defenders)

exactSuccessProb :: Battlefield -> Double
exactSuccessProb bf = error "todo"

main :: IO ()
main = do
  gen <- initStdGen
  print $ evalRand (invade $ Battlefield 3 10) gen
