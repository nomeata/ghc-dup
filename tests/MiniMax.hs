import Game

import Data.List
import Data.Function

data GameTree = Node State [(Int, GameTree)] deriving (Show)

allgames :: Player -> GameTree
allgames player = 
    genTree (initialState player)

genTree :: State -> GameTree
genTree s = Node s [ (m, genTree s) | m <- [1..6], Just s <- return (applyMove s m)]

value :: State -> Int
value s = getScore PlayerA s - getScore PlayerB s

miniMax :: Int -> GameTree -> Int 
miniMax 0 (Node s _) = value s
miniMax _ (Node s []) = value s
miniMax n (Node s ts) = select [ miniMax (n-1) t | (m,t) <- ts ]
    where select = if getPlayer s == PlayerA then maximum else minimum

decide :: Int -> GameTree -> Int
decide n (Node s ts) = fst $ select (compare `on` snd) [ (m, - (miniMax n t)) | (m,t) <- ts ]
    where select = if getPlayer s == PlayerA then maximumBy else minimumBy

main = do
    simulateGame2 (\s -> decide 7 (genTree s))
