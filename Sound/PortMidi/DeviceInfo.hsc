{-# OPTIONS_HADDOCK hide #-}

module Sound.PortMidi.DeviceInfo (
    DeviceInfo(..)
  , peekDeviceInfo
  ) where

import Foreign
import Foreign.C

#include "portmidi.h"

data DeviceInfo
  =  DeviceInfo
  { interface :: String
  , name      :: String
  , input     :: Bool
  , output    :: Bool
  , opened    :: Bool
  } deriving (Eq, Show)

peekDeviceInfo :: Ptr a -> IO DeviceInfo
peekDeviceInfo ptr = do
  s <- #{peek PmDeviceInfo, interf} ptr >>= peekCString
  u <- #{peek PmDeviceInfo, name} ptr >>= peekCString
  i <- #{peek PmDeviceInfo, input} ptr
  o <- #{peek PmDeviceInfo, output} ptr
  d <- #{peek PmDeviceInfo, opened} ptr
  return $ DeviceInfo s u (asBool i) (asBool o) (asBool d)
  where
    asBool :: #{type int} -> Bool
    asBool = (/= 0)
