-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model as M
import ExtraFunctions
import Consts
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as Game
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ pureStep secs gstate

pureStep :: Float -> GameState -> GameState
pureStep secs gState@(Menu _) = gState
pureStep secs gState = gState{elapsedTime = elapsedTime gState + secs}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input = return *. handleEvent

handleEvent :: Event -> GameState -> GameState
handleEvent (EventResize newSize) = \gstate -> gstate{screenSize = newSize}
handleEvent (EventKey key state _ _) = handleInput
  where 
    handleInput :: GameState -> GameState
    handleInput gstate = fromMaybe gstate $ fnLookup gstate
    fnLookup :: GameState -> Maybe GameState
    fnLookup gs = Map.lookup (state,key) keyFunctions >>= 
      \f -> return $ f gs
handleEvent _ = id 

keyFunctions :: Map.Map (KeyState, Key) (GameState  -> GameState)
keyFunctions = Map.fromList [
  ((Game.Up, moveUp), updateMove (updateDir M.Up)),
  ((Game.Down, moveUp), updateMove (toDir M.Up)),
  ((Game.Up, moveLeft), updateMove (updateDir M.Left)),
  ((Game.Down, moveLeft), updateMove (toDir M.Left)),
  ((Game.Up, moveRight), updateMove (updateDir M.Right)),
  ((Game.Down, moveRight), updateMove (toDir M.Right)),
  ((Game.Up, moveDown), updateMove (updateDir M.Down)),
  ((Game.Down, moveDown), updateMove (toDir M.Down))]

updateMove :: (Direction -> Direction)-> GameState -> GameState
updateMove mov gstate = gstate{gsplayer = player{movement = newMove}}
  where
    player :: Player
    player = gsplayer gstate
    newMove :: Direction
    newMove = mov (movement player)