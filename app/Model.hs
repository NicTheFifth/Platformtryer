module Model where
import Graphics.Gloss

data GameState = Menu {
                    screenSize :: (Int, Int)
                }
               | ActiveGame {
                    screenSize :: (Int, Int),
                    elapsedTime :: Float,
                    player :: Player,
                    platforms :: [Platform]
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
data VDirection = Up   | VNone | Down

class Picturable a where
    toPic :: a -> Picture 

class Positional a where
    pos :: a -> (Int,Int)

instance Positional Player where
    pos = pPos

instance Positional Platform where
    pos = plPos

toMenu :: GameState -> GameState
toMenu gState@(Menu _ ) = gState
toMenu gState = Menu $ screenSize gState
         
initialState :: (Int, Int) -> GameState
initialState = undefined