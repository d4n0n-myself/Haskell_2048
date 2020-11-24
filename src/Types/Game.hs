
{-# LANGUAGE DeriveGeneric #-}

module Types.Game (
  Move(..),
  PlayGrid(..),
  GameState(..),
--  initialState,
--  mainChecks,
  generateRandomTile,
  allMoves,
  maxValue,
  gameWon,
  updateGrid,
  printGrid,
  performMove,
  getEmptyCells,
  canPlay,
  createGameState,
  PlayerMove(..),
  moveMappingFromInt
) where

import System.Random
import Control.Monad.Random
import Data.Aeson
import Data.Maybe
import GHC.Generics
import Prelude hiding (Left, Right)
import Data.Char (toLower)
import Data.List
import System.Random
import Control.Lens ((&), (?~), mapped)
import Data.Swagger
    (
        ToSchema, declareNamedSchema,
        genericDeclareNamedSchema, defaultSchemaOptions,
        schema, description, example, ToParamSchema
    )
import Data.UUID (UUID, fromString)
import Data.UUID.V4 (nextRandom)

-- | Возможные ходы игрока
data Move =
    Up    -- | выполнить перемещение ячеек вверх
  | Down  -- | выполнить перемещение ячеек вниз
  | Left  -- | выполнить перемещение ячеек влево
  | Right -- | выполнить перемещение ячеек вправо
   deriving (Eq,Show,Read,Generic)

data PlayerMove = PlayerMove Move deriving (Eq, Show, Read, Generic)

instance FromJSON Move
instance ToJSON Move
instance ToSchema Move

instance FromJSON PlayerMove
instance ToJSON PlayerMove
instance ToSchema PlayerMove
--instance ToSchema Move where
--    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
--        & mapped.schema.example ?~ toJSON Left

-- | Игровое поле ( 4 * 4 ячейки )
data PlayGrid = PlayGrid { values :: [[Int]] } deriving (Eq,Show,Read,Generic)


instance FromJSON PlayGrid
instance ToJSON PlayGrid
instance ToSchema PlayGrid


-- | Состояние игры
data GameState = GameState
  { grid :: PlayGrid, uuid :: UUID }
   deriving (Eq,Show,Read,Generic)


instance FromJSON GameState
instance ToJSON GameState
instance ToSchema GameState


createGameState :: IO GameState
createGameState = do
  let initVals = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
  first <- generateRandomTile initVals
  second <- generateRandomTile first
  newUuid <- nextRandom
  return GameState { grid = PlayGrid { values = second }, uuid = newUuid }


-- Добавить случайную ячейку
generateRandomTile :: [[Int]] -> IO [[Int]]
generateRandomTile values = do
  let candidates = getEmptyCells values
  cell <- myGetRandom candidates -- получить ячейку для вставки
  let allValues = replicate 9 2 ++ [4]
  value <- myGetRandom allValues
  let res = updateGrid values cell value
  return res


-- Установить значение
updateGrid :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
updateGrid grid (row, col) val = do
  let beginning = take row grid -- строки до нужной
  let newRow = take col (grid!!row) ++ [val] ++ drop (col + 1) (grid!!row) -- создать новую строку
  let ending = drop (row + 1) grid -- строки после нужной
  beginning ++ [newRow] ++ ending


-- Выбрать случайный элемент из массива
myGetRandom :: [a] -> IO a
myGetRandom list = do
-- element / choice
  i <- randomRIO (0, length list-1)
  return (list !! i)


-- Коллекция допустимых ходов
allMoves :: [Move]
allMoves = [Left, Right, Up, Down]


-- Маппинг значений клавиш на Move
moveMapping :: [(Char, Move)]
moveMapping = zip "wasd" [Up, Left, Down, Right]


-- Маппинг значений на Move
moveMappingFromInt :: Int -> Move --[(Int, Move)]
moveMappingFromInt value = do
  let dictionary = zip [1,2,3,4] [Up, Left, Down, Right]
  case lookup value dictionary of
      Nothing -> error "Can't map move"
      Just x  -> x


-- Значение для победы
-- | Выигрыш наступает при сборке ячейки со значением 2048
maxValue :: Int
maxValue = 2048


-- Основные проверки игры
--mainChecks :: [[Int]] -> IO ()
--mainChecks grid
--  | canPlay grid = do
--      printGrid grid
--      if gameWon grid
--        then print "WIN"
--        else do
--          playerChosenMove <- loopWhileMoveNotMapped
--          let newGrid = performMove playerChosenMove grid
--          if grid /= newGrid
--            then do
--              new <- generateRandomTile newGrid
--              mainChecks new
--            else do
--              print "Nothing changed, make other move"
--              mainChecks grid
--  | otherwise = do
--      print "LOSE"


-- Выиграл ли игрок
gameWon :: [[Int]] -> Bool
gameWon grid = do
  let anyMaxValues = filter (== maxValue) (concat grid)
  not $ null anyMaxValues


-- Получить координаты пустых ячеек
getEmptyCells :: [[Int]] -> [(Int, Int)]
getEmptyCells grid = do
  let singleRow n = zip (replicate 4 n) [0..3]
  let coordinates = concatMap singleRow [0..3] -- сгенерировать двумерный [0..3]
  filter (\(row, col) -> (grid!!row)!!col == 0) coordinates


-- сложить строку влево
mergeToLeft :: [Int] -> [Int]
mergeToLeft row = do
  -- Data.Maybe catMaybes
  let filtered = filter (/= 0) row -- отсекаем 0
  let merged = combine filtered -- объединяем возможные
  let padding = replicate (length row - length merged) 0 -- создаем необходимый "хвост" до 4 элементов
  merged ++ padding


-- объединить ячейки 
combine :: [Int] -> [Int]
combine (first:second:tail)
  | first == second = first * 2 : combine tail
  | otherwise = first : combine (second:tail)
combine list = list


-- Проверка возможности выполнения действий
canPlay :: [[Int]] -> Bool
canPlay grid = do
-- изменить проверку так, чтобы учесть случай когда может что-то поменяться даже при всех занятых клетках
  let emptyCells = getEmptyCells grid
  let ln = length emptyCells
  ln > 0


-- Выполнить действие игрока
performMove :: Move -> [[Int]] -> [[Int]]
performMove Left = map mergeToLeft
performMove Right = map (reverse . mergeToLeft . reverse)
performMove Up = transpose . performMove Left  . transpose
performMove Down = transpose . performMove Right . transpose


-- вывод на экран
printGrid :: [[Int]] -> IO ()
printGrid value = do
  putStr "\n\n\n\n\n"
  mapData value


mapData :: [[Int]] -> IO ()
mapData (a:as) = do
  mapRow a
  if as /= []
    then mapData as
    else return ()


mapRow :: [Int] -> IO ()
mapRow (b:bs) = do
  putStr $ show b
  putStr "\t"
  if bs /= []
    then mapRow bs
    else putStr "\n"


-- Циклимся пока не найдем совпадение по moveMapping (по ключам WASD)
loopWhileMoveNotMapped :: IO Move
loopWhileMoveNotMapped = do
  inputChar <- getChar
  case lookup (toLower inputChar) moveMapping of
    Just x  -> return x
    Nothing -> loopWhileMoveNotMapped

