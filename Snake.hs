{-# OPTIONS_GHC -Wall #-}

import Data.List

import System.IO
import System.Timeout
import System.Random
import System.Console.ANSI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Loops
import Control.Applicative

type Vector = (Int, Int)

type MoveVector = (Int, Int, Int)

data State = State {
    board :: Int,
    snake1 :: [Vector],
    snake2 :: [Vector],
    fruit :: Maybe (Vector, StdGen),
    move1 :: Maybe MoveVector,
    move2 :: Maybe MoveVector
} deriving Show

main :: IO State
main = clearScreen
    >> initialState 
    >>= (iterateUntilM gameOver step)
               
oneSecond :: Int
oneSecond = (9 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

initialState :: IO State
initialState = getStdGen 
    >>= \stdGen -> return State {
        board = 40,
        snake1 = [(4, 0), (3, 0), (2, 0), (1, 0), (0, 0)],
    snake2 = [(4, 10), (3, 10), (2, 10), (1, 10), (0, 10)],
        fruit = randomElem (concat (buildBoard 40)) stdGen,
        move1  = Just (0, 1, 0),
    move2  = Just (1, 1, 0)
    }

randomElem :: [a] -> StdGen -> Maybe (a, StdGen)
randomElem [] _  = Nothing
randomElem xs inputStdGen  = Just (element, stdGen)
    where indexStdGenTuple = randomR (0, length xs - 1) inputStdGen
          index            = fst indexStdGenTuple
          stdGen           = snd indexStdGenTuple
          element          = xs !! index

newFruit :: State -> Maybe (Vector, StdGen)
newFruit (State { fruit = Nothing }) = Nothing
newFruit state@(State { fruit = Just (_, stdGen) })
    = randomElem validPositions stdGen
        where allPositions   = concat $ buildBoard $ board state
              validPositions = allPositions \\ snake1 state

step :: State -> IO State
step state = sample sampleLength getInput 
    >>= \ inputMove ->
        displayState $ updateState state (vectorFromChar inputMove)

displayState :: State -> IO State
displayState state = setCursorPosition 0 0 
    >> putStr (render state) 
    >> return state

vectorFromChar :: Maybe Char -> Maybe MoveVector
vectorFromChar (Just 'w') = Just (0, 0,  1)
vectorFromChar (Just 'a') = Just (0, -1,  0)
vectorFromChar (Just 's') = Just (0, 0, -1)
vectorFromChar (Just 'd') = Just (0, 1,  0)
vectorFromChar (Just 'i') = Just (1, 0,  1)
vectorFromChar (Just 'j') = Just (1, -1,  0)
vectorFromChar (Just 'k') = Just (1, 0, -1)
vectorFromChar (Just 'l') = Just (1, 1,  0)
vectorFromChar _          = Nothing

getInput :: IO Char
getInput = hSetEcho stdin False 
    >> hSetBuffering stdin NoBuffering
    >> getChar

gameOver :: State -> Bool
gameOver (State { snake1 = [] }) = True
gameOver (State { snake2 = [] }) = True
gameOver (State {
    board = boardSize,
    snake1 = (snakeHead1@(snakeHeadX1, snakeHeadY1):snakeBody1),
    snake2 = (snakeHead2@(snakeHeadX2, snakeHeadY2):snakeBody2)
})
    | snakeHeadX1 >= (boardSize*2) || snakeHeadX1 < 0 = True
    | snakeHeadY1 >= boardSize || snakeHeadY1 < 0 = True
    | snakeHead1 `elem` snakeBody1                = True
    | snakeHead2 `elem` snakeBody1                = True
    | snakeHeadX2 >= (boardSize*2) || snakeHeadX2 < 0 = True
    | snakeHeadY2 >= boardSize || snakeHeadY2 < 0 = True
    | snakeHead2 `elem` snakeBody1                = True
    | snakeHead2 `elem` snakeBody2                = True
    | otherwise                                 = False

render :: State -> String
render state
    = unlines $ applyBorder (board state)
              $ map (renderRow state)
              $ buildBoard (board state)

applyBorder :: Int -> [String] -> [String]
applyBorder size renderedRows
    = border ++ map (\row -> "I" ++ row ++ "I") renderedRows ++ border
        where border = [replicate ((size*2) + 1) '-']

renderRow :: State -> [Vector] -> String
renderRow state = map (characterForPosition state)

characterForPosition :: State -> Vector -> Char
characterForPosition state position
    | position `elem` snake1 state                = 'o'
    | position `elem` snake2 state                = 'x'
    | fruit state `fruitPositionEquals` position = '@'
    | otherwise                                  = ' '

fruitPositionEquals :: Maybe (Vector, StdGen) -> Vector -> Bool
fruitPositionEquals (Just (position, _)) vector = position == vector
fruitPositionEquals _ _                         = False

snake1HasFruitInMouth :: State -> Bool
snake1HasFruitInMouth state
    = fruit state `fruitPositionEquals` head (snake1 state)

snake2HasFruitInMouth :: State -> Bool
snake2HasFruitInMouth state
    = fruit state `fruitPositionEquals` head (snake2 state)

buildBoard :: Int -> [[(Int, Int)]]
buildBoard size
    = [[(x, y) | x <- [0 .. (size - 1)*2]] | y <- reverse [0 .. size - 1]]

updateState :: State -> Maybe MoveVector -> State
updateState state inputMove
        = updateFruit $ updateSnake1 $ updateSnake2 $ updateMove state inputMove

updateMove :: State -> Maybe MoveVector -> State
updateMove state@(State { move1 = Just moveVector1, move2 = Just moveVector2}) inputMove@(Just inputVector)
    | first inputVector == 0 && inputVector /= moveVectorOpposite moveVector1 
        = state { move1 = inputMove <|> move1 state }
    | first inputVector == 1 && inputVector /= moveVectorOpposite moveVector2 
        = state { move2 = inputMove <|> move1 state }
updateMove state _ = state

updateSnake1 :: State -> State
updateSnake1 = updateSnakeTail1 . updateSnakeHead1

updateSnake2 :: State -> State
updateSnake2 = updateSnakeTail2 . updateSnakeHead2

updateFruit :: State -> State
updateFruit state
    | snake1HasFruitInMouth state = state { fruit = newFruit state }
    | snake2HasFruitInMouth state = state { fruit = newFruit state }
    | otherwise                  = state

updateSnakeHead1 :: State -> State
updateSnakeHead1 state@(State { move1 = (Just vector) })
    = state { snake1 = head (snake1 state) `vectorAdd` vector : snake1 state }
updateSnakeHead1 state = state

updateSnakeHead2 :: State -> State
updateSnakeHead2 state@(State { move2 = (Just vector) })
    = state { snake2 = head (snake2 state) `vectorAdd` vector : snake2 state }
updateSnakeHead2 state = state

updateSnakeTail1 :: State -> State
updateSnakeTail1 state
    | snake1HasFruitInMouth state = state
    | otherwise                  = state { snake1 = init $ snake1 state }

updateSnakeTail2 :: State -> State
updateSnakeTail2 state
    | snake2HasFruitInMouth state = state
    | otherwise                  = state { snake2 = init $ snake2 state }

vectorAdd :: Vector -> MoveVector -> Vector
vectorAdd (x1, y1) (a, x2, y2) = (x1 + x2, y1 + y2)

moveVectorOpposite :: MoveVector -> MoveVector
moveVectorOpposite (z, x, y) = (z, -x, -y)

first :: MoveVector -> Int
first (z, x, y) = z

sample :: Int -> IO a -> IO (Maybe a)
sample n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
    | otherwise =
        concurrently (timeout n f) (threadDelay n) 
            >>= \ (result, _) -> return result