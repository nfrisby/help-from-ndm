module Test where

import System.Environment (getEnv)

import qualified Common
import Common (Fib(..))
import qualified SYB
import qualified Uni
import qualified Hand

import Criterion.Config
import Criterion.Main (defaultMainWith, nf, bgroup, bench)




-- To benchmark on 20..25
--   Ns=`ghc -e [20..25]` ACTION=bench ./Test
--
-- To validate on 20..25
--   Ns=`ghc -e [20..25]` ACTION=validate ./Test
main = do
  nss <- getEnv "Ns"
  if null nss then putStrLn "The Ns environment variable must be a list of positive integers."
    else let ns :: [Int]
             ns = read nss
         in getEnv "ACTION" >>= \s -> case s of
              "validate" -> mainvalid ns
              "bench" -> mainbench ns
              _ -> putStrLn "ACTION must be in {validate,bench}"

run f i = f (Fib (Lit i))

variants =
  ("Hand", Hand.reduce) :
  ("SYB", SYB.reduce) :
  ("Uni", Uni.reduce) :
  []

mainvalid ns = print $ and
  [ run f i == run Common.eval i | i <- ns, f <- map snd variants ]

mainbench ns =
  let myConfig = defaultConfig
                 { cfgPerformGC = ljust True
                 , cfgSummaryFile = ljust $ "summary.csv"
                 , cfgSamples = ljust 10
                 }
  in defaultMainWith myConfig (return ()) $ [
       bgroup (show i)
         [ bench s $ nf (run reducer) i | (s, reducer) <- variants ]
      | i <- ns ]
