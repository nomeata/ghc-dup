-- Taken from http://web.cs.wpi.edu/~cs4536/c06/Assignments/Game.hs
-- Game.hs

module Game(State, Player(PlayerA, PlayerB), otherPlayer, applyMove, initialState, getScore, nextStates, getPlayer, simulateGame, simulateGame2) where

import System.IO

data Player = PlayerA | PlayerB deriving Eq
data PlayerState = PlayerState Player Int [Int]
data State = State PlayerState PlayerState

instance Show Player where
  show PlayerA = "A"
  show PlayerB = "B"

instance Show PlayerState where
  show (PlayerState player score pits) =
    (show player) ++ ": " ++ (show score) ++ "; " ++ (show pits)

instance Show State where
  show s =
    "(A " ++ (show (getScore PlayerA s)) ++ ") " ++ (show (reverse (getPits PlayerA s))) ++
    "\n(B " ++ (show (getScore PlayerB s)) ++ ") " ++ (show (getPits PlayerB s))

otherPlayer :: Player -> Player
otherPlayer PlayerA = PlayerB
otherPlayer PlayerB = PlayerA

initialState :: Player -> State
initialState p = 
  State (PlayerState p 0 (replicate 6 4)) (PlayerState (otherPlayer p) 0 (replicate 6 4))

getScore :: Player -> State -> Int
getScore p (State (PlayerState a sa _) (PlayerState b sb _)) =
  if p == a then sa else sb

getPits :: Player -> State -> [Int]
getPits p (State (PlayerState a _ pa) (PlayerState b _ pb)) =
  if p == a then pa else pb

getPlayer :: State -> Player
getPlayer (State (PlayerState p _ _) _) = p

incStones :: PlayerState -> PlayerState
incStones p = distStones [1..6] p

incStonesBy :: Int -> PlayerState -> PlayerState
incStonesBy n p = (iterate incStones p) !! n

incScore :: Int -> PlayerState -> PlayerState
incScore i (PlayerState p s ps) = (PlayerState p (s + i) ps)

distStones :: [Int] -> PlayerState -> PlayerState
distStones is (PlayerState p s ps) =
  (PlayerState p s (map (\ (i, n) -> if (elem i is) then n + 1 else n) (zip (iterate (+ 1) 1) ps)))

isWinningState :: State -> Bool
isWinningState (State (PlayerState _ a _) (PlayerState _ b _)) =
  a >= 24 || b >= 24

isMoveValid :: Int -> Bool
isMoveValid n = elem n [1..6]

isMoveLegal :: State -> Int -> Bool
isMoveLegal _ n | not (isMoveValid n) = False
isMoveLegal s _ | (isWinningState s) = False
isMoveLegal s n = case s of
                    (State (PlayerState a sa pa) _) -> not $ (pa !! (n - 1)) == 0

applyMove :: State -> Int -> Maybe State
applyMove s n | not (isMoveLegal s n) = Nothing
applyMove s n = Just (applyMove' s' p')
  where
    (s', p') = case s of
                (State (PlayerState a sa pa) playerother) ->
                  ((State (PlayerState a sa (killPit pa)) playerother), pa !! (n - 1))
                  
    killPit xs = (take (n - 1) xs) ++ [0] ++ (drop n xs)
    
    applyMove' (State a b) p = 
      let (rounds, extras) = divMod p 13
          sinc             = rounds + (if (6 - n) < extras then 1 else 0)
          extras' = if n == 6
                      then extras - 1
                      else if null adist then 0 else extras - (last adist) + (head adist) - 2
          extras''  = if null bdist then 0 else extras' - (last bdist) + (head bdist) - 1
          adist     = [i | i <- [(n + 1)..6], i - n <= extras]
          bdist     = [i | i <- [1..6], i <= extras']
          adist'    = [i | i <- [1..6], i <= extras'']
          currP = ((incScore sinc) . (distStones adist) . (distStones adist') . (incStonesBy rounds) $ a)
          otherP = ((distStones bdist) . (incStonesBy rounds) $ b) in
        if extras == (6 - n + 1) then (State currP otherP) else (State otherP currP)
        --(State otherP currP)

nextStates :: State -> [State]
nextStates s = [s | (Just s) <- (map (applyMove s) [1..6])]

simulateGame :: IO ()
simulateGame = simulateGame' (Just (initialState PlayerA)) where
  simulateGame' Nothing = return ()
  simulateGame' (Just (State (PlayerState p s ps) b)) =
    do
      print (State (PlayerState p s ps) b)
      putStr ((show p) ++ ": ")
      move <- getLine
      move <- return (read move) :: IO (Int)
      s' <- return (applyMove (State (PlayerState p s ps) b) move)
      case s' of
         Nothing -> putStr "ERROR\n"
         (Just a) -> if (isWinningState a) 
                          then putStr ((show p) ++ " wins!\n")
                          else simulateGame' s'
      return ()

simulateGame2 :: (State -> Int) -> IO ()
simulateGame2 chooseMove = simulateGame' (Just (initialState PlayerA)) where
  simulateGame' Nothing = return ()
  simulateGame' (Just state@(State (PlayerState p s ps) b)) =
    do
      print state
      putStr ((show p) ++ ": ")
      hFlush stdout
      move <-
	  if p == PlayerB then
             do move <- getLine
		return (read move) :: IO (Int)
	  else
             do let move = (chooseMove state)
                print move
                return move
      s' <- return (applyMove (State (PlayerState p s ps) b) move)
      case s' of
         Nothing ->
             do putStr "ERROR\n"
                simulateGame' (Just state)
         (Just a) -> if (isWinningState a) 
                          then putStr ((show p) ++ " wins!\n")
                          else simulateGame' s'
      return ()

