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

-- Nossa MVar que salva os valores dos placares
type Score = MVar (Int, Int)

type Level = MVar Int

type MoveVector = (Int, Int, Int)

data State = State {
    -- Tamanho do board
    board :: Int,
    -- A posição de cada parte das cobrinhas
    snake1 :: [Vector],
    snake2 :: [Vector],
    -- A posição de cada bloco e fruta
    blocks :: [Vector],
    food :: [Vector],
    -- A key geradora de aleatorios
    std :: StdGen,
    -- Direção e sentido das cobras
    move1 :: Maybe MoveVector,
    move2 :: Maybe MoveVector,
    -- Pontuação de cada jogador
    points1 :: Int,
    points2 :: Int,
    level :: Int
} deriving Show

-- main
main= do
        score <- newMVar (0,0)
        level <- newMVar 1
        newGame level score 

-- Loop para vários jogos
newGame:: Level -> Score -> IO ()
newGame level score = do
    game level score
    putStrLn "O placar vai se manter, quer jogar novamente? (Digite 'n' para sair ou outra tecla para continuar)"
    resp <- getChar 
    if resp /= 'n' then do
        l <- takeMVar level
        putMVar level (l+1)
        newGame level score
    else do 
        putStrLn "Bye Bye!!!"

-- A inicalização da tela de jogo
game :: Level -> Score -> IO State
game level score = clearScreen
    >> initialState score level
    >>= (iterateUntilM gameOver (step score))
               
oneSecond :: Int
oneSecond = (9 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

-- Thread que controla o tempo e os inputs
sample :: Int -> IO a -> IO (Maybe a)
sample n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
    | otherwise =
        concurrently (timeout n f) (threadDelay n) 
            >>= \ (result, _) -> return result

-- Estado inicial de cada jogo
initialState :: Score -> Level -> IO State
initialState score level = do
                    (a,b) <- takeMVar score
                    putMVar score (a,b)
                    l <- takeMVar level
                    putMVar level l
                    stdGen <- getStdGen 
                    return State {
                board = 50,
                snake1 = [(4, 24), (3, 24), (2, 24), (1, 24)],
                snake2 = [(46, 27), (47, 27), (48, 27), (49, 27)],
                std = stdGen,
                food = [randomElem (concat (buildBoard 50)) stdGen],
                blocks = [],
                move1  = Just (0, 1, 0),
                move2  = Just (1, -1, 0),
                points1 = a,
                points2 = b,
                level = l
            }

-- Geração aleatória de elementos tanto pra frutas quanto para blocos
randomElem :: [a] -> StdGen -> a
randomElem xs inputStdGen = element
    where indexStdGenTuple = randomR (0, length xs - 1) inputStdGen
          element          = xs !! fst (indexStdGenTuple)

newFood :: State -> StdGen -> Vector
newFood state st
    = randomElem validPositions4 st
        where allPositions   = concat $ buildBoard $ board state
              validPositions1 = allPositions \\ snake1 state
              validPositions2 = validPositions1 \\ snake2 state
              validPositions3 = validPositions2 \\ blocks state
              validPositions4 = validPositions3 \\ food state

--Aqui gera pelo menos uma fruta
newFoods :: State -> Int -> [Vector]
newFoods state 0 = [newFood state (mkStdGen 1)]
newFoods state n = [newFood state (mkStdGen (n+1))] ++ newBlocks state (n-1)

--Aleatorizar as frutas
scrambleFoods :: [Vector] -> [Vector]
scrambleFoods f = map mix f

mix :: Vector -> Vector
mix (a,b) = ((a*a `mod` 50),(b*a `mod` 50))

newBlock :: State -> StdGen -> Vector
newBlock state st
    = randomElem validPositions4 st
        where allPositions   = concat $ buildBoard $ board state
              validPositions1 = allPositions \\ snake1 state
              validPositions2 = validPositions1 \\ snake2 state
              validPositions3 = validPositions2 \\ blocks state
              validPositions4 = validPositions3 \\ food state

newBlocks :: State -> Int -> [Vector]
newBlocks state 0 = [newBlock state (mkStdGen 0)]
newBlocks state n = [newBlock state (mkStdGen n)] ++ newBlocks state (n-1)

step :: Score -> State -> IO State
step score state = sample sampleLength getInput 
    >>= \ inputMove ->
        displayState score $ updateState state (vectorFromChar inputMove) 

-- O Score acessa a MVar e atualiza os seus valores
updateScore :: Score -> State -> IO ()
updateScore score state@( State {points1 = p1, points2 = p2}) = do
                                                                takeMVar score
                                                                putMVar score (p1,p2)

--Essa eh a impressão do jogo
displayState :: Score -> State -> IO State
displayState score state = setCursorPosition 0 0
    >> updateScore score state
    >> putStr (render state) 
    >> return state

render :: State -> String
render state
    = unlines $ applyBorder state
              $ map (renderRow state)
              $ buildBoard (board state)

applyBorder :: State -> [String] -> [String]
applyBorder state@(State { board = size, points1 = s1, points2= s2, blocks = b, food =f}) renderedRows
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
    | position `elem` food state                = '😐'
    | otherwise                                  = ' '

--Aqui analisa o que cada jogador inseriu
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

--Aqui se pega os inputs
getInput :: IO Char
getInput = hSetEcho stdin False 
    >> hSetBuffering stdin NoBuffering
    >> getChar

--Verifica se o jogo acabou ou não
gameOver :: State -> Bool
gameOver (State { snake1 = [] }) = True
gameOver (State { snake2 = [] }) = True
gameOver state 
    | death state > 0 = True
    | otherwise       = False

--Aqui verifica quem morreu
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
    | otherwise                                   = 0

snake1HasFoodInMouth :: State -> Bool
snake1HasFoodInMouth state
    = head (snake1 state) `elem` food state  

snake2HasFoodInMouth :: State -> Bool
snake2HasFoodInMouth state
    = head (snake2 state) `elem` food state  

buildBoard :: Int -> [[(Int, Int)]]
buildBoard size
    = [[(x, y) | x <- [0 .. (size - 1)]] | y <- reverse [0 .. size - 1]]

updateState :: State -> Maybe MoveVector -> State
updateState state inputMove
        = updateItens $ updateSnake1 $ updateSnake2 $ updateMove state inputMove

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

--Aqui dá um update nas frutas e nos blocos
updateItens :: State -> State
updateItens state@(State {points1 = s1, points2 = s2, food = f, blocks = b, std = rd, level = l})
    | snake1HasFoodInMouth state = state { food = scrambleFoods (f ++ newFoods state index1), blocks = b ++ newBlocks state index2, points1 = (s1+1),std = neo2}
    | snake2HasFoodInMouth state = state { food = scrambleFoods(f ++ newFoods state index1), blocks = b ++ newBlocks state index2, points2 = (s2+1),std = neo2}
    | otherwise                  = state
    where indexStdGenTuple1 = randomR (1, 3) (rd)
          index1            = (fst indexStdGenTuple1)
          neo              = snd indexStdGenTuple1
          indexStdGenTuple2 = randomR (0, 3*l) (neo)
          index2            = (fst indexStdGenTuple2)
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
    | snake1HasFoodInMouth state = state
    | otherwise                  = state { snake1 = init $ snake1 state }

updateSnakeTail2 :: State -> State
updateSnakeTail2 state
    | snake2HasFoodInMouth state = state
    | otherwise                  = state { snake2 = init $ snake2 state }

vectorAdd :: Vector -> MoveVector -> Vector
vectorAdd (x1, y1) (a, x2, y2) = (x1 + x2, y1 + y2)

moveVectorOpposite :: MoveVector -> MoveVector
moveVectorOpposite (z, x, y) = (z, -x, -y)

first :: MoveVector -> Int
first (z, x, y) = z
