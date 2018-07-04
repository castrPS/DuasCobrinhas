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

type Score = MVar (Int, Int)

type MoveVector = (Int, Int, Int)

data State = State {
    board :: Int,
    snake1 :: [Vector],
    snake2 :: [Vector],
    blocks :: [Vector],
    fruits :: [Vector],
    std :: StdGen,
    move1 :: Maybe MoveVector,
    move2 :: Maybe MoveVector,
    points1 :: Int,
    points2 :: Int
} deriving Show

main= do
        score <- newMVar (0,0)
        game score
        game score

game :: Score -> IO State
game score = clearScreen
    >> initialState score
    >>= (iterateUntilM gameOver (step score))
               
oneSecond :: Int
oneSecond = (9 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

initialState :: Score -> IO State
initialState score = do
                    (a,b) <- takeMVar score
                    putMVar score (a,b)
                    stdGen <- getStdGen 
                    return State {
                board = 50,
                snake1 = [(4, 0), (3, 0), (2, 0), (1, 0), (0, 0)],
                snake2 = [(20, 10), (19, 10), (18, 10), (17, 10), (16, 10)],
                std = stdGen,
                fruits = [randomElem (concat (buildBoard 50)) stdGen],
                blocks = [],
                move1  = Just (0, 1, 0),
                move2  = Just (1, 1, 0),
                points1 = a,
                points2 = b
            }

randomElem :: [a] -> StdGen -> a
randomElem xs inputStdGen = element
    where indexStdGenTuple = randomR (0, length xs - 1) inputStdGen
          element          = xs !! fst (indexStdGenTuple)

newFruit :: State -> StdGen -> Vector
newFruit state st
    = randomElem validPositions4 st
        where allPositions   = concat $ buildBoard $ board state
              validPositions1 = allPositions \\ snake1 state
              validPositions2 = validPositions1 \\ snake2 state
              validPositions3 = validPositions2 \\ blocks state
              validPositions4 = validPositions3 \\ fruits state

scrambleFruits :: [Vector] -> [Vector]
scrambleFruits f = map mix f

mix :: Vector -> Vector
mix (a,b) = ((a*a `mod` 50),(b*a `mod` 50))

newBlock :: State -> StdGen -> Vector
newBlock state st
    = randomElem validPositions4 st
        where allPositions   = concat $ buildBoard $ board state
              validPositions1 = allPositions \\ snake1 state
              validPositions2 = validPositions1 \\ snake2 state
              validPositions3 = validPositions2 \\ blocks state
              validPositions4 = validPositions3 \\ fruits state

newBlocks :: State -> Int -> [Vector]
newBlocks state 0 = [newBlock state (mkStdGen 0)]
newBlocks state n = [newBlock state (mkStdGen n)] ++ newBlocks state (n-1)

newFruits :: State -> Int -> [Vector]
newFruits state 0 = [newFruit state (mkStdGen 1)]
newFruits state n = [newFruit state (mkStdGen (n+1))] ++ newBlocks state (n-1)

step :: Score -> State -> IO State
step score state = sample sampleLength getInput 
    >>= \ inputMove ->
        displayState score $ updateState state (vectorFromChar inputMove) 

updateScore :: Score -> State -> IO ()
updateScore score state@( State {points1 = p1, points2 = p2}) = do
                                                                takeMVar score
                                                                putMVar score (p1,p2)

displayState :: Score -> State -> IO State
displayState score state = setCursorPosition 0 0
    -- >> updateScore score state
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
gameOver state 
    | death state > 0 = True
    | otherwise       = False

death :: State -> Int
death (State { snake1 = [] }) = 2
death (State { snake2 = [] }) = 1
death (State {
    board = boardSize,
    snake1 = (snakeHead1@(snakeHeadX1, snakeHeadY1):snakeBody1),
    snake2 = (snakeHead2@(snakeHeadX2, snakeHeadY2):snakeBody2),
    blocks = b
})
    | snakeHeadX1 >= boardSize || snakeHeadX1 < 0 = 2
    | snakeHeadY1 >= boardSize || snakeHeadY1 < 0 = 2
    | snakeHead1 `elem` snakeBody1                = 2
    | snakeHead1 `elem` snakeBody2                = 2
    | snakeHead1 `elem` b                   = 2
    | snakeHead2 `elem` snakeBody1                = 1
    | snakeHeadX2 >= boardSize || snakeHeadX2 < 0 = 1
    | snakeHeadY2 >= boardSize || snakeHeadY2 < 0 = 1
    | snakeHead2 `elem` snakeBody2                = 1
    | snakeHead2 `elem` b                   = 1
    | snakeHead2 == snakeHead1                    = 3
    | otherwise                                   = 0

render :: State -> String
render state
    = unlines $ applyBorder state
              $ map (renderRow state)
              $ buildBoard (board state)

applyBorder :: State -> [String] -> [String]
applyBorder state@(State { board = size, points1 = s1, points2= s2, blocks = b, fruits =f}) renderedRows
    = border ++ map (\row -> "I" ++ row ++ "I") renderedRows ++ border ++ score ++ text 
        where border = [replicate (size + 2) '-']
              score = ["👻 Fantasmas: " ++ show s1 ++" 👽 Aliens: " ++ show s2 ++ "\n"]
              text
                | death state == 0 = [""]
                | death state == 1 = ["👻 Os fantasmas sobreviveram! 👻"]
                | death state == 2 = ["👽 Os aliens sobreviveram! 👽 "]
                | death state == 3 = ["Os anjos nem precisaram tentar, pois os aliens e os fantasmas acabaram se destruindo!"]
                | otherwise = ["!!!"]

renderRow :: State -> [Vector] -> String
renderRow state = map (characterForPosition state)

characterForPosition :: State -> Vector -> Char
characterForPosition state position
    | position `elem` blocks state                = '👼'
    | position `elem` snake1 state                = '👻'
    | position `elem` snake2 state                = '👽'
    | position `elem` fruits state                = '😐'
    | otherwise                                  = ' '

snake1HasFruitInMouth :: State -> Bool
snake1HasFruitInMouth state
    = head (snake1 state) `elem` fruits state  

snake2HasFruitInMouth :: State -> Bool
snake2HasFruitInMouth state
    = head (snake2 state) `elem` fruits state  

buildBoard :: Int -> [[(Int, Int)]]
buildBoard size
    = [[(x, y) | x <- [0 .. (size - 1)]] | y <- reverse [0 .. size - 1]]

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
updateFruit state@(State {points1 = s1, points2 = s2, fruits = f, blocks = b, std = rd})
    | snake1HasFruitInMouth state = state { fruits = scrambleFruits (f ++ newFruits state index1), blocks = b ++ newBlocks state index2, points1 = (s1+1),std = neo2}
    | snake2HasFruitInMouth state = state { fruits = scrambleFruits(f ++ newFruits state index1), blocks = b ++ newBlocks state index2, points2 = (s2+1),std = neo2}
    | otherwise                  = state
    where indexStdGenTuple1 = randomR (1, 5) (rd)
          index1            = fst indexStdGenTuple1
          neo              = snd indexStdGenTuple1
          indexStdGenTuple2 = randomR (0, 3) (neo)
          index2            = fst indexStdGenTuple2
          neo2              = snd indexStdGenTuple1

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