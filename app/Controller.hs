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
handleEvent (EventResize newSize) gstate = gstate{screenSize = newSize}
handleEvent (EventKey key Game.Down _ _) gstate = inputKeyDown key gstate
handleEvent (EventKey key Game.Up _ _) gstate = inputKeyUp key gstate
handleEvent _ gState = gState --Otherwise keep the same

inputKeyUp :: Key -> GameState -> GameState
inputKeyUp (Char a) _ = undefined
inputKeyUp _ gState = gState -- Otherwise keep the same

inputKeyDown :: Key -> GameState -> GameState
inputKeyDown (Char a) gstate = moveKeyDown a gstate
inputKeyDown (SpecialKey KeySpace) gstate = gstate
inputKeyDown _ gState = gState -- Otherwise keep the same

moveKeyDown :: Char -> GameState -> GameState
moveKeyDown 'w' gstate = move $ moveV M.Down
moveKeyDown 'a' gstate = move $ moveH M.Left
moveKeyDown 's' gstate = move $ moveV M.Up
moveKeyDown 'd' gstate = move $ moveH M.Right
moveKeyDown _   gstate = gstate
  where
    player :: Player
    player = player gstate
    moveH x = (,) x (snd (movement player))
    moveV x = (,) (fst (movement player)) x
    move newMove = gstate{player = player{movement = newMove}}