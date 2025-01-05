module CSP where

import Data.Maybe ( fromJust )
import Control.Monad.CSP
    ( allCSPSolutions,
      constraint,
      constraint1,
      constraint2,
      constraint3,
      mkDV )
  
import qualified Data.HashMap.Strict as M
import Text.Pretty.Simple
    ( pPrintOpt,
      defaultOutputOptionsDarkBg,
      CheckColorTty(CheckColorTty),
      OutputOptions(outputOptionsCompact) )


main :: IO ()
main = putStrLn "Hello, use CSP to solve your problem !"


------------------------------------------------------
--  Solve TWO + TWO = FOUR : lettre inside [0..9]
--  TWO
--  TWO
-------
-- FOUR

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ _ []    = return ()
mapAllPairsM_ _ [_]   = return ()
mapAllPairsM_ f (a:l) = mapM_ (f a) l >> mapAllPairsM_ f l


solveTwoPlusTwo :: [[Int]]
solveTwoPlusTwo = allCSPSolutions $ do
  dvT <- mkDV [0 .. 9]
  dvW <- mkDV [0 .. 9]
  dvO <- mkDV [0 .. 9]
  dvF <- mkDV [0 .. 9]
  dvU <- mkDV [0 .. 9]
  dvR <- mkDV [0 .. 9]
  dvR1 <- mkDV [0 .. 1]
  dvR2 <- mkDV [0 .. 1]

  assertAllDifferentConstraints [dvT, dvO, dvW, dvF, dvU, dvR]
  constraint3 oPlusOegaleR dvO dvR dvR1
  constraint wPlusWegaleU [dvW, dvU, dvR1, dvR2]
  constraint tPlusTegaleO [dvT, dvO, dvF, dvR2]

  return [dvT, dvW, dvO, dvU, dvF, dvU, dvR, dvR1, dvR2]

    where
        oPlusOegaleR o r r1 =         r == o + o - 10*r1
        wPlusWegaleU (w:u:r1:r2:_) =  u == r1 + w + w - 10*r2
        wPlusWegaleU _ = False
        tPlusTegaleO (t:o:f:r2:_)  =  o == r2 + t+t - 10*f
        tPlusTegaleO _ = False
        assertAllDifferentConstraints =  mapAllPairsM_ (constraint2 (/=))
        

resolveTwoPlusTwo :: IO ()
resolveTwoPlusTwo = do
    let sol = solveTwoPlusTwo
    let t = length sol
    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = True} sol
    putStrLn $ "Total number : " ++ show t


------------------------------------------------------
-- Solve Einstein problem

solveEinstein :: [[String]] -> [[[Int]]]
solveEinstein listVar = allCSPSolutions $ do

  dvs <- mapM (mapM ( \a -> mkDV [0 :: Int .. 4] )) listVar
  let listNameDvs = zipWith zip listVar dvs
  let dictDvs = M.fromList $ concat listNameDvs

  mapM_ assertAllDifferentConstraints dvs

  constraint2egale dictDvs "anglais" "rouge"
  constraint2egale dictDvs "espagnol" "chien"
  constraint2egale dictDvs "japonais" "peintre"
  constraint2egale dictDvs "italien" "the"
  constraint1agauche dictDvs "norvegien"
  constraint2egale dictDvs "vert" "cafe"
  constraint2adroite dictDvs "vert" "blanc"
  constraint2egale dictDvs "sculpteur" "escargot"
  constraint2egale dictDvs "diplomate" "jaune"
  constraint1aumilieu dictDvs "lait"
  constraint2estvoisin dictDvs "norvegien" "bleu"
  constraint2egale dictDvs "violoniste" "jus"
  constraint2estvoisin dictDvs "renard" "medecin"
  constraint2estvoisin dictDvs "cheval" "diplomate"

  return dvs

    where
        estvoisin x y = x==y+1 || x==y-1
        adroite x y = x==y+1
        agauche = (==(0 :: Int))
        aumilieu = (==(2 :: Int))

        assertAllDifferentConstraints =  mapAllPairsM_ (constraint2 (/=))

        constraint2egale dict v1 v2 = constraint2 (==) (fromJust $ M.lookup v1 dict ) (fromJust $ M.lookup v2 dict)
        constraint1agauche dict v = constraint1 agauche (fromJust $ M.lookup v dict )
        constraint2adroite dict v1 v2 = constraint2 adroite (fromJust $ M.lookup v1 dict ) (fromJust $ M.lookup v2 dict)
        constraint1aumilieu dict v = constraint1 aumilieu (fromJust $ M.lookup v dict )
        constraint2estvoisin dict v1 v2 = constraint2 estvoisin (fromJust $ M.lookup v1 dict ) (fromJust $ M.lookup v2 dict)



resolveEinstein :: IO ()
resolveEinstein = do
    let sol = solveEinstein allListVars
    let solName = map (zipWith zip allListVars) sol
    let l = length sol
    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = True} solName
    putStrLn $ "Total number : " ++ show l


allListVars :: [[String]]
allListVars = [   ["anglais", "espagnol", "japonais", "italien", "norvegien"]
                , ["rouge", "vert", "blanc", "jaune", "bleu"]
                , ["the", "cafe", "lait", "jus", "eau"]
                , ["peintre", "sculpteur", "diplomate", "violoniste", "medecin"]
                , ["chien", "escargot", "renard", "cheval", "zebre"]
              ]



