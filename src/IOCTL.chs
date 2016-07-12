{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

#include "sys/ioctl.h"

module IOCTL (
      writeOnTTY
    , runOnTTY
    ) where

import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String

import qualified Data.ByteString as B

foreign import ccall "sys/ioctl.h ioctl"
    c_ioctl :: CInt -> CInt -> Ptr CChar -> IO ()

runOnTTY :: ByteString -> IO ()
runOnTTY bs = writeOnTTY (bs <> "\n")

writeOnTTY :: ByteString -> IO ()
writeOnTTY bs = unsafeUseAsCString bs (doTheThing l)
  where l = B.length bs

doTheThing :: Int -> CString -> IO ()
doTheThing l str = mapM_ writeByte [0 .. (l-1)]
  where writeByte i  = c_ioctl 0 code (plusPtr str i)
        code = {#const TIOCSTI#}
