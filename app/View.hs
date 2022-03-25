-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gameState = undefined

instance Picturable GameState where
    toPic (Menu a) = undefined
    toPic (ActiveGame size time player platforms) = undefined