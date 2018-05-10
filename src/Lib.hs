{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Control.Lens
import Data.Default
import Graphics.Vty.Input.Events

data AppState = AppState
  { _vx :: Float
  , _vy :: Float
  , _x :: Float
  , _y :: Float
  } deriving (Show, Eq)

makeLenses ''AppState

instance Default AppState where
  def = AppState {_vx = 0, _vy = 0, _x = 0, _y = 0}

data Tick =
  Tick

handleEvent :: AppState -> BrickEvent () Tick -> EventM () (Next AppState)
handleEvent s (AppEvent Tick) = continue $ tick s
handleEvent s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handleEvent s (VtyEvent (EvKey KUp _)) = continue (s & vy .~ 2)
handleEvent s (VtyEvent (EvKey KRight _)) = continue (s & vx .~ 3)
handleEvent s (VtyEvent (EvKey KUpLeft _)) = continue (s & vy .~ 2 & vx .~ (-3))
handleEvent s (VtyEvent (EvKey KUpRight _)) = continue (s & vy .~ 2 & vx .~ 3)
handleEvent s (VtyEvent (EvKey KLeft _)) = continue (s & vx .~ (-3))
handleEvent s _ = continue s

tick :: AppState -> AppState
tick s = s & gravity & moveY & moveX & limitY & friction & stop
  where
    gravity = vy %~ subtract 0.5
    friction = vx *~ 0.7
    stop =
      vx %~ \vx' ->
        if abs vx' < 0.01
          then 0
          else vx'
    moveY = y +~ (s ^. vy)
    moveX = x +~ (s ^. vx)
    limitY = y %~ max 0

initApp :: AppState -> EventM n AppState
initApp s = return s

renderApp :: AppState -> [Widget ()]
renderApp = pure . renderBouncer

gridHeight :: Int
gridHeight = 10

gridWidth :: Int
gridWidth = 100

renderBouncer :: AppState -> Widget ()
renderBouncer s =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Bounce") $ vBox rows
  where
    rows :: [Widget ()]
    rows = fmap hBox . reverse $ getRow <$> [0 .. gridHeight]
    getRow y' = getCell y' <$> [0 .. gridWidth]
    getCell y' x' =
      if (s ^. y . to floor) == y' && (s ^. x . to floor) == x'
        then str "o"
        else str "-"
