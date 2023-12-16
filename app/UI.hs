{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Data.Maybe ()

import Snake

import Brick (neverShowCursor, App (..), Widget, BrickEvent (..), EventM, AttrMap, customMain)
import Brick.BChan (newBChan, writeBChan)
import Brick.Types (modify, put)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Data.Sequence ()
import qualified Data.Sequence as S
import Linear.V2 ()
import Lens.Micro ()
import Brick.Main (halt)

-- Types

data Tick = Tick

type Name = ()

data Cell = Snake | Food | Empty

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

theMap :: AttrMap
theMap = undefined

drawUI :: Game -> [Widget Name]
drawUI = undefined

main :: IO ()
main = do
    chan <- newBChan 10
    forkIO . forever $ do
        writeBChan chan Tick
        threadDelay 100000
    g <- initGame
    let builder = mkVty V.defaultConfig
    initialVty <- builder
    void $ customMain initialVty builder (Just chan) app g

handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent (AppEvent Tick)                       = modify step
handleEvent (VtyEvent (V.EvKey V.KUp []))         = modify $ turn North
handleEvent (VtyEvent (V.EvKey V.KDown []))       = modify $ turn South
handleEvent (VtyEvent (V.EvKey V.KRight []))      = modify $ turn East
handleEvent (VtyEvent (V.EvKey V.KLeft []))       = modify $ turn West
handleEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = modify $ turn North
handleEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = modify $ turn South
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = modify $ turn East
handleEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = modify $ turn West
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = void   $ liftIO initGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent _                                     = return ()
