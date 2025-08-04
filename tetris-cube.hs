import           Control.Lens    (view,over)
import           Control.Monad   (guard)
import           Data.Ix         (range,inRange)
import           Data.List       (foldl',nub,sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid     (Endo(..))
import           Linear.Matrix   ((!*),identity)
import           Linear.V3

type V = V3 Int
type Shape = [V]

shapes :: [Shape]
shapes = nub $ map (normalize . flip map tee . (!*)) rots where
  tee = [V3 0 0 0,V3 1 0 0,V3 2 0 0,V3 1 1 0]
  -- we don't bother filtering for positive transformations because
  -- the tee shape is invariant under inversion.
  rots = flip appEndo identity . mconcat . map Endo <$> sequence
    [ view <$> [_xyz,_xzy,_yxz,_yzx,_zxy,_zyx]
    , [id, over _x negate]
    , [id, over _y negate]
    , [id, over _z negate]
    ]
  normalize s =
    let s'@(m:_) = sort s
    in map (subtract m) s'

solve :: [Map V Int]
solve = go Map.empty 0 (range bds) where
  bds = (V3 0 0 0,V3 5 5 5)
  go m _ [] = pure m
  go m i (v:vs)
    | v `Map.member` m = go m i vs
    | otherwise = do
        s <- map (v +) <$> shapes
        guard (all (inRange bds) s)
        guard (all (`Map.notMember` m) s)
        let m' = foldl' (\m_ v' -> Map.insert v' i m_) m s
        go m' (i+1) vs

main :: IO ()
main =  print $ head solve
