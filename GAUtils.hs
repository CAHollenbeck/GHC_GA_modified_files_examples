{-# LANGUAGE DeriveGeneric #-} -- Celeste

module GAUtils (
        activate, -- UoE
        getModelFromFlag,
        NNLink'
) where

import System.IO.Unsafe (unsafePerformIO)  
import GHC.Generics (Generic) -- UoE
import Data.ByteString.Lazy as L (readFile) -- UoE
import qualified Data.Binary as B --(get, put) -- UoE
-- import qualified Prelude as P -- UoE
import Prelude

-- UoE begin ====================================================

import Outputable

data NNActivation = Sigmoid | Relu | Identity deriving (Show, Generic)


data NNNode' = NNNode' { links' :: [NNLink'],
                         idx'   :: Int,  -- actually an int, but double for multiplication
                         activation'  :: NNActivation,-- Double -> Double, -- activation function, often sigmoid
                         is_input' :: Bool,
                         bias'     :: Double
                       } deriving (Show, Generic)

data NNNode = NNNode { 
                       links       :: [NNLink],
                       idx         :: String,  -- actually an int, but double for multiplication
                       activation  :: String,-- Double -> Double, -- activation function, often sigmoid
                       is_input :: String,
                       bias     :: String
                     } deriving (Show, Generic)


data NNLink' = NNLink' { innode' :: NNNode',
                         weight' :: Double,
                         outnode' :: Int } deriving (Show, Generic)

data NNLink = NNLink { innode  :: NNNode,
                       weight  :: String,
                       outnode :: String } deriving (Show, Generic)


getNNNode :: NNNode -> NNNode'
getNNNode n = (NNNode' {  idx'         = read (idx n) :: Int
                        , links'       = getNNLinks (links n)
                        , activation'  = getNNActivation (activation n)
                        , bias'        = read (bias n) :: Double
                        , is_input'    = getInput (is_input n)})


instance B.Binary NNNode
instance B.Binary NNLink

instance B.Binary NNNode' where
  get = do 
                   ls     <- B.get
                   i      <- B.get
                   act    <- B.get
                   input <- B.get
                   b      <- B.get
                   return (NNNode' {  links'        = ls
                                     , idx'         = i
                                     , activation'  = act
                                     , is_input'    = input
                                     , bias'        = b
                                     })


instance B.Binary NNLink' where
   get = do
                   i <- B.get
                   w <- B.get
                   o <- B.get
                   return (NNLink' {   innode'  = i
                                     , weight'  = w
                                     , outnode' = o })


instance B.Binary NNActivation

getNNActivation :: String -> NNActivation
getNNActivation "sigmoid"  = Sigmoid
getNNActivation "relu" = Relu
getNNActivation "identity" = Identity

getResponse :: String -> Double
getResponse "none" = 0
getResponse x      = read x :: Double

getInput :: String -> Bool
getInput "True"  = True
getInput "False" = False

getNNLinks :: [NNLink] -> [NNLink']
getNNLinks [] = []
getNNLinks (x:xs) = [(getNNLink x)] ++ (getNNLinks xs)

getNNLink :: NNLink -> NNLink'
getNNLink l = (NNLink' {   weight' = read (weight l) :: Double
                          , innode' = (getNNNode (innode l))
                          , outnode' = read (outnode l) :: Int })

-- Pass a feature vector to an NN & return activation
-- fs : The list of feature weights
activate :: NNLink' -> [Double] -> Double
activate l fs = case (is_input' n) of
                     True   ->  val     
                                -- Input nodes coming from JSON are not zero-indexed. This seems cleanest.
                                where nodenum  = ((idx' n)*(-1) - 1)-- trace (show ((idx' n)*(-1) - 1)) ((idx' n)*(-1) - 1)
                                      inputval = fs!!nodenum -- trace "FTRS " (trace (show fs) fs!!nodenum)
                                      val      = (((weight' l) * inputval) + (bias' n)) 
                     -- False  ->  (activ ((Prelude.foldr (+) 0 inputs) + (bias' n))) * (weight' l)
                     False  ->  valnoftr
                                       where valnoftr = ((activ ((aggregation_sum inputs) + (bias' n))) * (weight' l))
                 where n = (innode' l)
                       activ  = (case (activation' n) of
                                       Relu     -> relu
                                       Sigmoid  -> sigmoid
                                       Identity -> identity) -- should never be reached
                       inputs = (map (`activate` fs) (links' n)) -- TODO missing weights for hidden nodes

sigmoid :: Double -> Double
sigmoid x = (1 / (1 + exp(1)**(-x))) -- Note: ** is floating-point exponentiation

relu :: Double -> Double
relu x = if x > 0
           then x 
         else 0

aggregation_sum :: [Double] -> Double
aggregation_sum inputs = foldr (+) 0 inputs

-- This is how NEAT pre-processed the input to the activation
-- keeping the input between -60 and 60
prepX :: Double -> Double
prepX x = let z = if (x * 5) < 60
                  then (x * 5)
                  else 60
          in if (z > -60) then z else -60

-- activation function for input nodes (i.e., no activation function)
-- should never be reached
identity :: Double -> Double
identity x = x

getModelFromFlag :: String -> Maybe NNLink'
getModelFromFlag s = Just (getNNLink (NNLink
                            (B.decode (unsafePerformIO (L.readFile s)) :: NNNode)
                                 "1"
                                 "none")) 
-- UoE end ======================================================
