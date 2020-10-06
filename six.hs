{-# LANGUAGE FlexibleContexts #-}

-- module Main where

import Data.Int (Int8)
import Data.Word (Word64)
import Data.Bits

import Data.List
import qualified Data.Set as S

import Control.Applicative
import Control.Monad.List
import Control.Monad.State

import Debug.Trace

data Word256 = W256 !Word64 !Word64 !Word64 !Word64 deriving (Show,Eq,Ord)
instance Bits Word256 where
  (.&.) =        error "(.&.) is undefined"
  (W256 a b c d) .|. (W256 e f g h) =
    W256 (a .|. e) (b .|. f) (c .|. g) (d .|. h)
  xor =          error "xor is undefined"
  complement (W256 a b c d) = W256 (complement a) (complement b)
                                   (complement c) (complement d)
  shift =        error "shift is undefined"
  rotate =       error "rotate is undefined"
  bitSize =      error "bitSize is undefined"
  bitSizeMaybe = error "bitSizeMaybe is undefined"
  isSigned _ = False
  testBit (W256 a b c d) i | i < 64 = testBit a i
                           | i < 128 = testBit b (i - 64)
                           | i < 192 = testBit c (i - 128)
                           | i < 256 = testBit d (i - 192)
  bit i | i <  64 = W256 (bit i)      0            0             0
        | i < 128 = W256    0    (bit $ i-64)      0             0
        | i < 192 = W256    0         0       (bit $ i-128)      0
        | i < 256 = W256    0         0            0        (bit $ i-192)
  popCount (W256 a b c d) = popCount a + popCount b + popCount c + popCount d
instance FiniteBits Word256 where
  finiteBitSize _ = 256

data V3 = V3 { vX :: !Int8, vY ::  !Int8, vZ :: !Int8 } deriving Eq
instance Show V3 where
  show (V3 x y z) = "V" ++ show x ++ show y ++ show z

shape = [ V3 0 0 0, V3 1 0 0, V3 2 0 0, V3 1 1 0 ]
rShapes = nub $ map (flip map shape) transforms

transforms :: [V3 -> V3]
transforms = do
  cs <- permutations [vX,vY,vZ]
  [p1,p2,p3] <- mapM (\f -> [f, negate . f]) cs
  return $ \v -> V3 (p1 v) (p2 v) (p3 v)

toI (V3 x y z) = ((fromIntegral z * 6) + fromIntegral y) * 6 + fromIntegral x
fromI i = V3 x y z where (i',x) = fromIntegral i `divMod` 6
                         (z,y) = fromIntegral i' `divMod` 6

stEmpty = W256 0 0 0 0
stFull = W256          16777215 18446744073709551615
           18446744073709551615 18446744073709551615

(bdsA,bdsB,bdsC) = (6,6,6)
inBounds (V3 x y z) = x >= 0 && y >= 0 && z >= 0 &&
                      x < bdsA && y < bdsB && z < bdsC

(V3 a b c) !+! (V3 d e f) = V3 (a+d) (b+e) (c+f)

type St = Word256
type Shape = [V3]
newtype Solution = Solution [Shape] deriving Show

solve :: [Solution]
solve = evalState (go stEmpty []) S.empty where
  go :: St -> [Shape] -> State (S.Set St) [Solution]
  go st bt -- | traceShow (st,take 1 bt) False = undefined
           -- | popCount st == 16 = return $ pure $ Solution bt
           | st == stFull = return $ pure $ Solution $ reverse bt
           | otherwise = do
    cl <- get
    -- traceShow (S.size cl) (return ())
    if st `S.member` cl then return empty else do
      let i = countTrailingZeros (complement st)
          r = fromI i
      when (True || vZ r >= 3) $ modify' (S.insert st)
      foldr (<|>) [] <$> forM (map (r !+!) <$> rShapes) (\s -> runListT $ do
        let is = map toI s
        let st' = foldl' setBit st (map fromIntegral is)
        ListT $ return $ do
          guard (all inBounds s)
          guard (all (not . testBit st) is)
        ListT $ go st' (s : bt)
        )

tr x = traceShow x x

main = print (head solve)
