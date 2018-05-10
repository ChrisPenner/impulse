{-# LANGUAGE ViewPatterns #-}

module Lib where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Graphics.Vty.Input.Events

data AppState =
  AppState Float
           Float

data Tick =
  Tick

handleEvent :: AppState -> BrickEvent () Tick -> EventM () (Next AppState)
handleEvent (AppState v y) (AppEvent Tick) =
  continue (AppState (v - 1) (max (y + v) 0))
handleEvent s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handleEvent (AppState _ y) (VtyEvent (EvKey (KChar ' ') _)) =
  continue (AppState 3 y)
handleEvent s _ = continue s

initApp :: AppState -> EventM n AppState
initApp s = return s

renderApp :: AppState -> [Widget ()]
renderApp (AppState _ y) = pure $ renderBouncer y

renderBouncer :: Float -> Widget ()
renderBouncer (floor -> n) =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Bounce") $ vBox rows
  where
    rows :: [Widget ()]
    rows =
      reverse
        [ if n == r
          then str "*"
          else str "-"
        | r <- [0 .. 10 :: Int]
        ]
