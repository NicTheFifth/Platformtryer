module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (800, 800) (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              (initialState (400, 400))    -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
