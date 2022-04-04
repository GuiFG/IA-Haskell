module NeuralNetwork where

import Util
import Data.Matrix
import Control.DeepSeq
import qualified Debug.Trace as T

data NeuralNetwork = NeuralNetwork { inputNodes   :: Int
                                   , hiddenNodes  :: Int
                                   , outputNodes  :: Int
                                   , learningRate :: Float
                                   , weightsInputHidden  :: Matrix Float
                                   , weightsHiddenOutput :: Matrix Float
                                   , activationFunction  :: Float -> Float
                                   }

instance NFData NeuralNetwork where
    rnf nn = deepseq (weightsInputHidden nn) $ deepseq (weightsHiddenOutput nn) ()

-- sig(t) = 1 / (1 + e^(-t))
sigmoid :: Float -> Float
sigmoid v = (1/) $ (1+) $ exp $ negate v

subtractOne :: Float -> Float
subtractOne x = 1 - x

scaledList :: [Float] -> [Float]
scaledList = map (\x -> (x / 255.0) * 0.99 + 0.01)

targetList :: Int -> Int -> [Float]
targetList n = replaceValue list 0.99
  where
    list = replicate n 0.01

getTarget :: [Float] -> Int
getTarget = (+1) . round . head

nMatrixString :: Matrix Float -> String
nMatrixString m = "(" ++ show nLine ++ ", " ++ show nCols ++ ")"
  where
    nCols = ncols m
    nLine = nrows m


forward :: NeuralNetwork -> [Float] -> Matrix Float
forward nn xs = do
    let input        = fromList (inputNodes nn) 1 xs
    let activation   = multScalar $ activationFunction  nn
    let hiddenOutput = activation $ multStd2 (weightsInputHidden nn) input
    activation $ multStd2 (weightsHiddenOutput nn) hiddenOutput


updateIA :: NeuralNetwork -> [Float] -> [Float] -> NeuralNetwork
updateIA nn xs ys = do
    let input        = fromList (inputNodes nn) 1 xs
    let target       = fromList (outputNodes nn) 1 ys
    let lr           = learningRate nn
    let activation   = multScalar $ activationFunction  nn
    let hiddenOutput = activation $ multStd2 (weightsInputHidden nn) input
    let finalOutput  = activation $ multStd2 (weightsHiddenOutput nn) hiddenOutput
    let outputErrors = subtractMatrix target finalOutput
    let hiddenErrors = multStd2 (transpose $ weightsHiddenOutput nn) outputErrors
    let dwho         = multZipWithMatrix outputErrors $ multZipWithMatrix finalOutput $ multScalar subtractOne finalOutput
    let newWho       = sumZipWithMatrix (weightsHiddenOutput nn) $ multScalar (lr*) $ multStd2 dwho $ transpose hiddenOutput
    let dwih         = multZipWithMatrix hiddenErrors $ multZipWithMatrix hiddenOutput $ multScalar subtractOne hiddenOutput
    let newWih       = sumZipWithMatrix (weightsInputHidden nn) $ multScalar (lr*) $ multStd2 dwih $ transpose input
    createNeuralNetwork (inputNodes nn) (hiddenNodes nn) (outputNodes nn) (learningRate nn) newWih newWho (activationFunction nn)


trainIA :: NeuralNetwork -> Matrix Float -> NeuralNetwork
trainIA nn m
  | length xs > 0 = deepseq newNn $ updateIA newNn input target
  | otherwise     = nn
    where
      newNn  = trainIA nn $ fromListsSafe xss
      target = targetList (outputNodes nn) $ (+1) $ getTarget xs
      input  = scaledList . tail $ xs
      xss    = tail l
      xs     = head l
      l      = toLists m


train :: NeuralNetwork -> Matrix Float -> Int -> Int -> NeuralNetwork
train nn m c g = do
  if c <= g then ("Generation: " ++ show c) `T.trace` deepseq newNn  $ train newNn m (c+1) g
    else newNn
  where
    newNn = trainIA nn m

createNeuralNetwork :: Int -> Int -> Int -> Float -> Matrix Float -> Matrix Float -> (Float -> Float) -> NeuralNetwork
createNeuralNetwork i h o l wi wo f = NeuralNetwork {
                                inputNodes          = i
                              , hiddenNodes         = h
                              , outputNodes         = o
                              , learningRate        = l
                              , weightsInputHidden  = wi
                              , weightsHiddenOutput = wo
                              , activationFunction  = f
                              }


query :: NeuralNetwork -> [Float] -> [Float]
query nn input = map (\xs -> head xs) result
  where
    result = toLists $ forward nn input


test :: NeuralNetwork -> Matrix Float -> [Int]
test nn m
  | length xs > 0 = (if resultValue == target then [1] else [0]) ++ test nn (fromListsSafe xss)
  | otherwise      = []
  where
    resultValue = indexValue value list 0
    value       = maximum list
    list        = query nn input
    target      = getTarget xs
    input       = scaledList . tail $ xs
    xss         = tail l
    xs          = head l
    l           = toLists m

testIA :: NeuralNetwork -> Matrix Float -> Float
testIA nn m =  100 * result / size
  where
    size      = fromIntegral (length scoreList) :: Float
    result    = fromIntegral (sum scoreList)    :: Float
    scoreList = test nn m
