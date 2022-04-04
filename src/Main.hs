module Main where

import Util
import NeuralNetwork
import Control.DeepSeq

inodes, hnodes, onodes, generations :: Int
lr, mean :: Float

inodes      = 784
hnodes      = 200
onodes      = 10
generations = 3
lr          = 0.3
mean        = 0.0

pathTrain :: FilePath
pathTrain = "train/mnist_train_short.csv"

pathTest :: FilePath
pathTest = "test/mnist_test_short.csv"


ia :: NeuralNetwork
ia  = NeuralNetwork { inputNodes  = inodes
                   , hiddenNodes  = hnodes
                   , outputNodes  = onodes
                   , learningRate = lr
                   , weightsInputHidden  = generateRandomMatrix (hnodes, inodes) (mean, (fromIntegral hnodes) ** (-0.5))
                   , weightsHiddenOutput = generateRandomMatrix (onodes, hnodes) (mean, (fromIntegral onodes) ** (-0.5))
                   , activationFunction  = sigmoid
                   }


main :: IO ()
main = do
  contents <- readFile pathTrain
  let trainList = getMatrixFromString . words $ contents
  putStrLn "Training ..."
  let trainedIA = train ia trainList 1 generations
  deepseq trainedIA $ putStrLn "Train completed."
  testContents <- readFile pathTest
  putStrLn "Testing ..."
  let testList = getMatrixFromString . words $ testContents
  let performance = testIA trainedIA testList
  putStrLn $ "Performance = " ++ (show performance) ++ "%"
