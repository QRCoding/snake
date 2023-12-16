{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (void, forever)
import Control.Monad.IO.Class ()
import Control.Concurrent (forkIO, threadDelay)
import Data.Maybe ()

import Snake

import Brick (neverShowCursor, App (..), Widget, BrickEvent (..), EventM, AttrMap, customMain)
import Brick.Types (modify)
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Data.Sequence ()
import qualified Data.Sequence as S
import Linear.V2 ()
import Lens.Micro ()

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
handleEvent (AppEvent Tick)                            = modify step
-- handleEvent g (VtyEvent (V.EvKey V.KUp []))         = turn North g
-- handleEvent g (VtyEvent (V.EvKey V.KDown []))       = turn South g
-- handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
-- handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
-- handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
-- handleEvent g _                                     = continue g

