{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.State
import           Data.List
import           System.Exit
import           System.Random.Shuffle

data Suit
  = Spade
  | Club
  | Heart
  | Diamond
  deriving (Enum, Show)

data Card =
  Card Int
       Suit

instance Show Card where
  show (Card 1 suit)   = show suit ++ "_A"
  show (Card 11 suit)  = show suit ++ "_J"
  show (Card 12 suit)  = show suit ++ "_Q"
  show (Card 13 suit)  = show suit ++ "_K"
  show (Card num suit) = show suit ++ "_" ++ show num

data AppState = AppState
  { dealer :: [Card]
  , player :: [Card]
  , cards  :: [Card]
  } deriving (Show)

newtype AppT m a = AppT
  { unAppT :: StateT AppState m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadRandom
             , MonadState AppState
             )

runAppT :: (Monad m) => AppState -> AppT m a -> m a
runAppT st act = evalStateT (unAppT act) st

allCards :: [Card]
allCards = [Card num suit | num <- [1 .. 13], suit <- [Spade ..]]

initialState :: AppState
initialState = AppState {dealer = [], player = [], cards = allCards}

initCards :: (MonadRandom m) => AppT m ()
initCards = do
  AppState {..} <- get
  cards <- shuffleM cards
  put AppState {..}

dealerDraw :: (Monad m) => AppT m ()
dealerDraw = do
  AppState {..} <- get
  let dealer_new = head cards : dealer
  let cards_new = tail cards
  put AppState {dealer = dealer_new, player, cards = cards_new}

playerDraw :: (Monad m) => AppT m ()
playerDraw = do
  AppState {..} <- get
  let player_new = head cards : player
  let cards_new = tail cards
  put AppState {dealer, player = player_new, cards = cards_new}

sumCards :: [Card] -> Int
sumCards = foldr func 0
  where
    func (Card num _) acc =
      acc +
      if num > 10
        then 10
        else num

sumDealerHands :: (Monad m) => AppT m Int
sumDealerHands = gets $ sumCards . dealer

sumPlayerHands :: (Monad m) => AppT m Int
sumPlayerHands = gets $ sumCards . player

showHands :: (MonadIO m) => Bool -> AppT m ()
showHands doShowDetail = do
  AppState {..} <- get
  liftIO $ do
    putStrLn "==========="
    if doShowDetail
      then do
        putStr "dealer: "
        print dealer
        putStr "sum: "
        print $ sumCards dealer
      else do
        putStr "dealer: "
        print $ head dealer
    putStr "player: "
    print player
    putStr "sum: "
    print $ sumCards player
    putStrLn "==========="

showHandsCovered :: (MonadIO m) => AppT m ()
showHandsCovered = showHands False

showHandsUncovered :: (MonadIO m) => AppT m ()
showHandsUncovered = showHands True

preparePhase :: (MonadRandom m) => AppT m ()
preparePhase = do
  initCards
  dealerDraw >> dealerDraw
  playerDraw >> playerDraw

playerPhase :: (MonadIO m) => AppT m ()
playerPhase = do
  playerLoop
  playerSum <- sumPlayerHands
  when (playerSum > 21) lose
  where
    playerLoop = do
      showHandsCovered
      ans <- liftIO $ putStrLn "draw? [y/n]" >> getLine
      when ("y" `isPrefixOf` ans) $ do
        playerDraw
        playerHands <- sumPlayerHands
        if playerHands > 21
          then showHandsCovered >> liftIO (putStrLn "player burst")
          else playerLoop

dealerPhase :: (MonadIO m) => AppT m ()
dealerPhase = do
  dealerSum <- sumDealerHands
  when (dealerSum < 17) $ dealerDraw >> dealerPhase

win :: (MonadIO m) => AppT m ()
win = liftIO $ putStrLn "you won" >> exitSuccess

draw :: (MonadIO m) => AppT m ()
draw = liftIO $ putStrLn "draw" >> exitSuccess

lose :: (MonadIO m) => AppT m ()
lose = liftIO $ putStrLn "you lost" >> exitSuccess

evalCondition :: (MonadIO m) => AppT m ()
evalCondition = do
  playerSum <- sumPlayerHands
  dealerSum <- sumDealerHands
  case (playerSum, dealerSum) of
    (21, 21) -> draw
    (n, m)
      | m > 21 -> win
      | n == m -> draw
      | n > m -> win
      | otherwise -> lose

main :: IO ()
main =
  runAppT initialState $ do
    preparePhase
    playerPhase
    dealerPhase
    showHandsUncovered
    evalCondition
