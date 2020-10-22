module Types.Game (
  Move,
  PlayGrid,
  GameState,
  mkMove,
  mkMove1
) where

import Prelude hiding (Left, Right)

-- | Возможные ходы игрока
data Move =
  Up
    -- ^ выполнить перемещение ячеек вверх
  | Down
    -- ^ выполнить перемещение ячеек вниз
  | Left
    -- ^ выполнить перемещение ячеек влево
  | Right
    -- ^ выполнить перемещение ячеек вправо
  deriving (Eq,Show,Read)

-- | маппинг чисел
mkMove :: Int -> Maybe Move
mkMove x
  | x == 1 = Just (Up)
  | x == 2 = Just (Down)
  | x == 3 = Just (Left)
  | x == 4 = Just (Right)
  | otherwise = Nothing

-- | маппинг чисел
mkMove1 :: String -> Maybe Move
mkMove1 x
  | x == "w" = Just (Up)
  | x == "a" = Just (Down)
  | x == "s" = Just (Left)
  | x == "d" = Just (Right)
  | otherwise = Nothing

-- | Игровое поле
data PlayGrid = PlayGrid [[Int]] deriving (Eq,Show,Read)

-- | Состояние игры
data GameState = GameState
  { grid :: PlayGrid }
   deriving (Eq,Show,Read)