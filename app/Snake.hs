{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Snake (initGame, Game, step, turn, Direction (..)) where

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Linear.V2 (V2(..), _x, _y)
import qualified Data.Sequence as S
import Data.Sequence (Seq(..), (<|))
import Control.Monad (guard)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import System.Random (Random(randomRs), newStdGen)

-- Types

data Game = Game
    { _snake  :: Snake
    , _dir    :: Direction
    , _food   :: Coord
    , _foods  :: Stream Coord
    , _dead   :: Bool
    , _paused :: Bool
    , _score  :: Int
    , _locked :: Bool
    } deriving (Show)

type Coord = V2 Int
type Snake = Seq Coord

data Stream a = a :| Stream a
    deriving (Show)

data Direction = North | South | East | West
    deriving (Eq, Show)

makeLenses ''Game

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do

    -- Make sure the game isn't paused or over
    MaybeT $ guard . not <$> orM [use paused, use dead]

    -- Unlock from last directional turn
    MaybeT . fmap Just $ locked .= False

    -- die (moved into boundary), eat (moved into food), or move (move into space)
    die <|> eatFood <|> MaybeT (Just <$> modify move)

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g@Game { _snake = (s :|> _) } = g & snake .~ (nextHead g <| s)
move _                             = error "Snakes can't be empty!"

nextHead :: Game -> Coord
nextHead Game { _dir = d, _snake = (a :<| _) }
    | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
    | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
    | d == East  = a & _x %~ (\x -> (x + 1) `mod` width)
    | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)
nextHead _ = error "Snakes can't be empty!"

height, width :: Int
height = 20
width  = 20

eatFood :: MaybeT (State Game) ()
eatFood = do
    MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> use food
    MaybeT . fmap Just $ do
        modifying score (+ 10)
        get >>= \g -> modifying snake (nextHead g <|)
        nextFood

nextFood :: State Game ()
nextFood = do
    (f :| fs) <- use foods
    foods .= fs
    use snake >>= (\case
        True -> nextFood
        False -> food .= f) . elem f

die :: MaybeT (State Game) ()
die = do
    MaybeT . fmap guard $ elem <$> (nextHead <$> get) <*> use snake
    MaybeT . fmap Just $ dead .= True

turn :: Direction -> Game -> Game
turn d g = if g ^. locked
    then g
    else g & dir %~ turnDir d & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
            | c `elem` [East, West]   && n `elem` [North, South] = n
            | otherwise = c

initGame :: IO Game
initGame = do
    (f :| fs) <- fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
    let xm = width `div` 2
        ym = height `div` 2
        g = Game
            { _snake = S.singleton (V2 xm ym)
            , _food = f
            , _foods = fs
            , _score = 0
            , _dir = North
            , _dead = False
            , _paused = True
            , _locked = False
            }
    return $ execState nextFood g

fromList :: [a] -> Stream a 
fromList = foldr (:|) (error "Streams must be infinite") 
