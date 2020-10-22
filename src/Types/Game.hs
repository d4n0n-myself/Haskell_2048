module Types.Game (
  Move,
  PlayGrid(..),
  GameState(..),
  mkMoveFromInt,
  mkMoveFromStr,
  initialState
) where

import Prelude hiding (Left, Right)

-- | Возможные ходы игрока
data Move =
    Up    -- | выполнить перемещение ячеек вверх
  | Down  -- | выполнить перемещение ячеек вниз
  | Left  -- | выполнить перемещение ячеек влево
  | Right -- | выполнить перемещение ячеек вправо
  deriving (Eq,Show,Read)

-- | Получить Move из числа если оно входит в множество [1..4]
mkMoveFromInt :: Int -> Maybe Move
mkMoveFromInt x
  | x == 1 = Just (Up)
  | x == 2 = Just (Down)
  | x == 3 = Just (Left)
  | x == 4 = Just (Right)
  | otherwise = Nothing

-- | Получить Move из числа если оно входит в множество [ W, A, S, D ]
mkMoveFromStr :: String -> Maybe Move
mkMoveFromStr x
  | x == "w" = Just (Up)
  | x == "a" = Just (Down)
  | x == "s" = Just (Left)
  | x == "d" = Just (Right)
  | otherwise = Nothing


-- | Игровое поле ( 4 * 4 ячейки )
data PlayGrid = PlayGrid [[Int]] deriving (Eq,Show,Read)

-- | Выигрыш наступает при сборке ячейки со значением 2048

-- | Состояние игры
data GameState = GameState
  { grid :: PlayGrid }
   deriving (Eq,Show,Read)

initialState :: GameState
initialState = GameState { grid = PlayGrid [[1], [2]] }