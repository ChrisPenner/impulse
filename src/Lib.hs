{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Control.Lens
import Data.Default
import Data.Fixed
import Graphics.Vty.Input.Events

data AppState = AppState
  { _v :: Float
  , _x :: Float
  , _y :: Float
  , _theta :: Float
  } deriving (Show, Eq)

makeLenses ''AppState

instance Default AppState where
  def = AppState {_v = 0, _x = 0, _y = 0, _theta = 0}

data Tick =
  Tick

handleEvent :: AppState -> BrickEvent () Tick -> EventM () (Next AppState)
handleEvent s (AppEvent Tick) = continue $ tick s
handleEvent s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handleEvent s (VtyEvent (EvKey KUp _)) = continue (s & v +~ 0.1)
handleEvent s (VtyEvent (EvKey KRight _)) = continue (s & theta +~ (-0.1))
handleEvent s (VtyEvent (EvKey KLeft _)) = continue (s & theta +~ 0.1)
handleEvent s _ = continue s

tick :: AppState -> AppState
tick s = s & moveX & moveY & wrapY & wrapX & stop
  where
    moveY = y +~ sin (s ^. theta) * (s ^. v)
    moveX = x +~ cos (s ^. theta) * (s ^. v)
    friction = v *~ 0.9
    stop =
      v %~ \v' ->
        if abs v' < 0.01
          then 0
          else v'
    wrapX = y %~ (`mod'` fromIntegral gridHeight)
    wrapY = x %~ (`mod'` fromIntegral gridWidth)

initApp :: AppState -> EventM n AppState
initApp s = return s

renderApp :: AppState -> [Widget ()]
renderApp = pure . renderBouncer

gridHeight :: Int
gridHeight = 30

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
