{-# LANGUAGE GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

module Dup (Box(Box), dup, deepDup, deepDupFun) where

import GHC.Exts
import Debug.Trace
import System.Environment
import System.Mem
--import GHC.HeapView hiding (Box)
import Control.Monad

import System.IO.Unsafe

-- This is a datatype that has the same layout as Ptr, so that by
-- unsafeCoerce'ing, we obtain the Addr of the wrapped value
data Box a = Box a

-- This is for turning an a to something we can pass to a primitive operation

aToWord# :: Any -> Word#
aToWord# a = case Box a of mb@(Box _) -> case unsafeCoerce# mb :: Word of W# addr -> addr

wordToA# :: Word# -> Box Any
wordToA# a = unsafeCoerce# (W# a) :: Box Any

-- This is for actually calling the primitive operation

foreign import prim "dupClosure" dupClosure :: Word# -> Word#

{-# NOINLINE dup #-}
dup :: a -> Box a
dup a =
    -- unsafePerformIO (getClosureData a >>= print . tipe . info ) `seq`
    case dupClosure (aToWord# (unsafeCoerce# a)) of { x ->
    case wordToA# x of { Box x' -> Box (unsafeCoerce# x') }}

foreign import prim "deepDupClosure" deepDupClosure :: Word# -> Word#

deepDupFun :: a -> a
deepDupFun a =
    -- unsafePerformIO (putStrLn "" >> getClosureData a >>= \x -> (print x >> (mapM_ (getBoxedClosureData >=> print) (allPtrs x)) )) `seq`
    case deepDupClosure (aToWord# (unsafeCoerce# a)) of { x ->
    case wordToA# x of { Box x' ->
        -- unsafePerformIO (getClosureData x' >>= \x -> (print x >> (mapM_ (getBoxedClosureData >=> print) (allPtrs x)) )) `seq`
        unsafeCoerce# x'
    }}
{-# NOINLINE deepDupFun #-}

deepDup :: a -> Box a
deepDup a =
    -- Without -O, and without the following line breaks deepDup somehow:
    --unsafeDupablePerformIO (return ()) `seq`

    -- unsafePerformIO (putStrLn "" >> getClosureData a >>= \x -> (print x >> (mapM_ (getBoxedClosureData >=> print) (allPtrs x)) )) `seq`
    case deepDupClosure (aToWord# (unsafeCoerce# a)) of { x ->
    case wordToA# x of { Box x' ->
        -- unsafePerformIO (getClosureData x' >>= \x -> (print x >> (mapM_ (getBoxedClosureData >=> print) (allPtrs x)) )) `seq`
        -- unsafePerformIO (performGC) `seq`
        --unsafePerformIO (return ()) `seq`
        Box (unsafeCoerce# x')
    }}
{-# NOINLINE deepDup #-}

