module Test where

import System.Environment (getEnv)

import qualified Common

import qualified SYB; import qualified Data.Generics as SYB
import qualified Uni
import qualified TYB
import qualified Hand

import Criterion.Config
import Criterion.Main (defaultMainWith, nf, bgroup, bench)

import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (unGen)
import System.Random (mkStdGen)





drops = [10,94] -- excluding these two b/c they are small

-- manual_pruning of the random values
hand_prune = w where
  w _ [] = []
  w k (x : xs) | k `elem` drops = w (k + 1) xs
               | otherwise = x : w (k + 1) xs

mkcs n = hand_prune 0 $ unGen (sequence $ replicate (n + length (filter (<=n) drops)) arbitrary) (mkStdGen 280578) 70


main = do
  n <- ((id :: Int -> Int) . read) `fmap` getEnv "N"
  let cs = mkcs n
  getEnv "ACTION" >>= \s -> case s of
    "validate" -> mainvalid cs
    "bench" -> mainbench cs
    "stats" -> mainstats cs
    _ -> putStrLn "ACTION must be in {validate,bench}"

run f = f 1

variants =
  ("TYB", TYB.increase) :
  ("Hand", Hand.increase) :
  ("SYB", SYB.increase) :
  ("Uni", Uni.increase) :
  []

mainvalid cs = mapM_ (maybe (return ()) print) $
  [ if run f i == run Hand.increase i then Nothing else Just (s, n)
      | (n, i) <- zip [0..] cs, (s, f) <- variants ]

mainbench cs =
  let myConfig = defaultConfig
                 { cfgPerformGC = ljust True
                 , cfgSummaryFile = ljust $ "summary.csv"
                 , cfgSamples = ljust 10
                 }
  in defaultMainWith myConfig (return ()) $ [
       bgroup (show i)
         [ bench s $ nf (run reducer) c | (s, reducer) <- variants ]
      | (i, c) <- zip [0..] cs ]

mainstats cs =
  mapM_ (\x -> let n = countNodes x
                   s = countSalaries x
               in putStrLn $ show n ++ "\t" ++ show s ++ "\t" ++ show (div' s n)) cs
  where countSalaries = SYB.everything (+) (SYB.mkQ 0 $ \Common.S{} -> 1)
        countNodes = SYB.everythingBut (+) (\x -> (1 :: Int, SYB.stopper x))

div' :: Int -> Int -> Double
div' x y = toEnum x / toEnum y
