{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module UI (main) where

import Control.Monad (void, forever)
import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent (forkIO, threadDelay)
import Data.Maybe ()
import Control.Lens ()

import Snake

import Brick
    (neverShowCursor, App (..), Widget, BrickEvent (..)
    , EventM, AttrMap, customMain, halt, padRight, Padding (..)
    , (<+>), hLimit, vBox, padTop, str, withAttr, emptyWidget
    , AttrName, withBorderStyle, padAll, attrName, hBox, on, fg, attrMap
    )
import Brick.Types (modify)
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Linear.V2 (V2 (..))
import Lens.Micro ((^.))

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
theMap = attrMap V.defAttr
    [ (snakeAttr, V.blue `on` V.blue)
    , (foodAttr, V.red `on` V.red)
    , (gameOverAttr, fg V.red `V.withStyle` V.bold)]

drawUI :: Game -> [Widget Name]
drawUI g = 
    [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
    . B.borderWithLabel (str "Snake")
    $ vBox rows
    where
        rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
        cellsInRow y = [drawCoord (V2 x y)  | x <- [0..width-1]]
        drawCoord    = drawCell. cellAt
        cellAt c
            | c `elem` g ^. snake = Snake
            | c == g ^. food      = Food
            | otherwise           = Empty

drawCell :: Cell -> Widget Name 
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr  cw 
drawCell Empty = withAttr emptyAttr cw 

cw :: Widget Name
cw = str " "

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = attrName "snake"
foodAttr  = attrName "food"
emptyAttr = attrName "empty"

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
    $ vBox [ drawScore (g ^. score)
           , padTop (Pad 2) $ drawGameOver (g ^. dead)
           ]

drawGameOver :: Bool -> Widget Name
drawGameOver d = 
    if d
        then withAttr gameOverAttr . C.hCenter $ str "GAME OVER"
        else emptyWidget

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver" 

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
    . B.borderWithLabel (str "Score")
    . C.hCenter
    . padAll 1
    . str $ show n
    
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
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = void (liftIO initGame)
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent _                                     = return ()

