import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad (forM_)

-- For TextSegmentation
import Data.Map(fromList)

import HyloDP
import Fibonacci
import Knapsack
import EditDistance
import LongestCommonSubsequence
import RandomWalk
import TextSegmentation

testFibonacci =
  describe "Fibonacci" $ do
    let problem = fibDPProblem 100
    it "returns 100th Fibonacci number" $ do
        (dpSolve problem :: Integer) `shouldBe` (354224848179261915075 :: Integer)

testKnapsack=
  describe "Knapsack" $ do
    let capacity = 100
        items = [Item 20 40, Item 30 20, Item 40 50, Item 50 60, Item 20 10]
        kpDPP = KSP capacity items
        problem = ksDPProblem kpDPP
    it "best score" $ do
        (dpSolve problem :: TMax Value) `shouldBe` (TMax 100)
    it "best solution" $ do
        (dpSolve problem :: BestSolution Item (TMax Value)) `shouldBe`
            (BestSolution (Just [Item {value = 30, weight = 20},Item {value = 50, weight = 60},Item {value = 20, weight = 10}]) (TMax 100))

testEditDistance =
  describe "EditDistance" $ do
    let x = "train"
        y = "basin"
        edDPP = (x, y)
        problem = edDPProblem edDPP
    it "ed \"train\" \"basin\"" $ do
        (ed x y) `shouldBe` 3
    it "dpSolve problem :: TMin Int" $ do
        (dpSolve problem :: TMin Int) `shouldBe` (TMin 3)
    it "dpSolve problem :: BestSolution EDOperation (TMin Int)" $ do
        (dpSolve problem :: BestSolution EDOperation (TMin Int)) `shouldBe`
            (BestSolution (Just [Replace 't' 'b',Replace 'r' 'a',Replace 'a' 's',Keep 'i',Keep 'n']) (TMin 3))

testLongestCommonSubsequence =
  describe "LongestCommonSubsequence" $ do
    forM_ [("train", "rain", "rain"), ("aliada", "alada", "alada"), ("abcd", "efg", "")] $ \(xs, ys, sol) ->
      it ("For: " ++ show xs ++ " " ++ show ys) $ do
        (dpSolve $ lcsDPProblem xs ys :: BestSolution Char (TMax Int)) `shouldBe` (BestSolution (Just sol) (TMax $ length sol))

testRandomWalk =
  describe "RandomWalk" $ do
    let rwDPP = (RW 0 2 6)
        problem = rwDPProblem rwDPP
    it "Probability" $ do
        (dpSolve problem :: Probability) `shouldBe` (Probability 0.453125)

testTextSegmentation =
  describe "TextSegmentation" $ do
    let dictionary = fromList [ ("a", 0.05)
                          , ("acas", 0.01)
                          , ("as", 0.03)
                          , ("ca", 0.01)
                          , ("cas", 0.05)
                          , ("casaca", 0.05)
                          , ("de", 0.05)
                          , ("e", 0.02)
                          , ("la", 0.12)
                          , ("lacas", 0.05)
                          , ("nada", 0.07)
                          , ("sacad", 0.02)
                          , ("aca", 0.01)
                          , ("ad", 0.01)
                          , ("asa", 0.02)
                          , ("cadena", 0.06)
                          , ("casa", 0.08)
                          , ("da", 0.05)
                          , ("den", 0.02)
                          , ("en", 0.11)
                          , ("laca", 0.05)
                          , ("na", 0.01)
                          , ("saca", 0.05)
                          ]
    it "best sol for 'lacasacadenada'" $ do
        let problem = tsDPProblem dictionary "lacasacadenada"
        (dpSolve problem :: BestSolution String (MaxProd Probability)) `shouldBe`
            (BestSolution (Just ["la","casa","cadena","da"]) (MaxProd (Probability 2.88e-5)))
    it "best sol for 'ended'" $ do
        let problem = tsDPProblem dictionary "ended"
        (dpSolve problem :: BestSolution String (MaxProd Probability)) `shouldBe`
            (BestSolution Nothing (MaxProd (Probability 0.0)))

main :: IO ()
main = hspec $ do
    testFibonacci
    testKnapsack
    testEditDistance
    testLongestCommonSubsequence
    testRandomWalk
    testTextSegmentation
