import Data.Word
import Data.List
import Data.Bits
import Data.Function
import System.Environment
import System.Mem
import System.IO

import GHC.Dup

data Tree = Node Word32 [Tree]
firstChild (Node _ (t:_)) = t

t1 = tree 1
tree n = Node n (map tree [n * 3, n * 5, n * 7, n * 9])


depth = 4

solveDup t = case dup t of Box t -> solve t

solveDeepDup t = case deepDup t of Box t -> solve t

solve' (Node n ts) = n :
    solve' (fst (maximumBy (compare `on` snd) [ (t, rate' depth t) | t <- ts ]))

solve (Node n ts) = n :
    solve (fst (maximumBy (compare `on` snd) [ (t, rate depth t) | t <- ts ]))

--rate d t = rate' d t
rate' d t = case dup t of Box t2 -> rate d t2

rate 0 (Node n _) = popCount n
rate d (Node _ ts) = maximum (map (rate (d-1)) ts)

ht :: [Word32] -> Word32
ht xs = last xs + head xs

ht' :: [Word32] -> Word32
ht' xs = case dup xs of Box xs' -> last xs' + head xs

ht'' :: [Word32] -> Word32
ht'' xs = case deepDup xs of Box xs' -> last xs' + head xs

main = do
    [n] <- getArgs 
    let k = read n
    print k
    performGC
    let t = tree k
    -- let l = [1..(100000000)]
    --print (take 5 l)
    -- hFlush stdout
    -- performGC
    --print $ ht'' l
    --print (head l)

    --hFlush stdout

    --t `seq` return ()
    --print $ rate 1 t

    print $ solve t !! 10000
    print $ solve t !! 10000
    hFlush stdout
    print $ solve t !! 1
    


