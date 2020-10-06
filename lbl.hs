newtype Coord = Coord Int
type Level = Array (Coord,Coord)
data Next = Zero | One | L | I

floorCoords = ((1,1),(6,6))
pureLevel = listArray floorCoords . repeat
flatLevel = pureLevel Zero
emptyLevel = pureLevel Nothing

fillLevel prev = go  where
  
