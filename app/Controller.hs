-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model as M
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as Game
import ExtraFunctions

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
handleEvent (EventKey key Game.Down _ _) = inputKeyDown key
handleEvent (EventKey key Game.Up _ _) = inputKeyUp key
handleEvent _ = id 

inputKeyUp :: Key -> GameState -> GameState
inputKeyUp (Char a) = moveKeyUp a
inputKeyUp _ = id

moveKeyUp :: Char -> GameState -> GameState
moveKeyUp 'w' = updateMove (updateDir M.Down)
moveKeyUp 'a' = updateMove (updateDir M.Left)
moveKeyUp 's' = updateMove (updateDir M.Up)
moveKeyUp 'd' = updateMove (updateDir M.Right)
moveKeyUp _   = id

inputKeyDown :: Key -> GameState -> GameState
inputKeyDown (Char a) = moveKeyDown a
inputKeyDown _ = id

moveKeyDown :: Char -> GameState -> GameState
moveKeyDown 'w' = updateMove (toDir M.Down)
moveKeyDown 'a' = updateMove (toDir M.Left)
moveKeyDown 's' = updateMove (toDir M.Up)
moveKeyDown 'd' = updateMove (toDir M.Right)
moveKeyDown _   = id

updateMove :: (Direction -> Direction)-> GameState -> GameState
updateMove mov gstate = gstate{gsplayer = player{movement = newMove}}
  where
    player :: Player
    player = gsplayer gstate
    newMove :: Direction
    newMove = mov (movement player)