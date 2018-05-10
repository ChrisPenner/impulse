module Main where

import Brick
import Brick.BChan
import Control.Concurrent
import Control.Monad
import Data.Default
import Graphics.Vty as V
import Lib

main :: IO AppState
main = do
  chan <- newBChan 10
  _ <-
    forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 100000
  customMain (V.mkVty V.defaultConfig) (Just chan) app def

app :: App AppState Tick ()
app =
  App
    { appDraw = renderApp
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = initApp
    , appAttrMap = const (attrMap mempty [])
    }
