module Model where
import Graphics.Gloss

data GameState = Menu {
                    screenSize :: (Int, Int)
                }
               | ActiveGame {
                    screenSize :: (Int, Int),
                    elapsedTime :: Float,
                    gsplayer :: Player,
                    gsplatforms :: [Platform]
                }

data Player = Player {
                    pPos :: (Int, Int),
                    movement :: Direction
}

data Platform = Platform {
                    plPos :: (Int,Int),
                    size :: Int
}
type Direction = (HDirection, VDirection)

data HDirection = Left | HNone | Right
  deriving (Eq)
data VDirection = Up   | VNone | Down
  deriving (Eq)

class Directional a  where
  toDir :: a -> Direction -> Direction
  updateDir :: a -> Direction -> Direction

class Picturable a where
  toPic :: a -> Picture 

class Positional a where
  pos :: a -> (Int,Int)

instance Directional HDirection where
  toDir x dir = (x,snd dir)
  updateDir x dir@(x1,_) | x == x1 = (HNone,snd dir)
                         | otherwise = dir

instance Directional VDirection where
  toDir y dir = (fst dir,y)
  updateDir y dir@(_,y1) | y == y1 = (fst dir,VNone)
                         | otherwise = dir

instance Positional Player where
    pos = pPos

instance Positional Platform where
    pos = plPos

toMenu :: GameState -> GameState
toMenu gState@(Menu _ ) = gState
toMenu gState = Menu $ screenSize gState
         
initialState :: (Int, Int) -> GameState
initialState = undefined