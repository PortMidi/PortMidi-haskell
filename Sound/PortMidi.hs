{- |
    Interface to PortMidi
-}

{-# OPTIONS_GHC -fglasgow-exts #-}

module Sound.PortMidi ( 
  -- * Data Types
    PMError(..)
  , PMStream
  , DeviceInfo(..)
  , DeviceID
  , PMMsg(..)
  , PMEvent(..) 
  -- * Constants
  , filterActive
  , filterSysex
  , filterClock
  , filterPlay
  , filterTick
  , filterFD
  , filterUndefined
  , filterReset
  , filterRealtime
  , filterNote
  , filterChannelAftertouch
  , filterPolyAftertouch
  , filterAftertouch
  , filterProgram
  , filterControl
  , filterPitchBend
  , filterMTC
  , filterSongPosition
  , filterSongSelect
  , filterTune
  , filterSystemCommon
  -- * PortMid functions
  , initialize
  , terminate
  , hasHostError
  , getErrorText
  , countDevices
  , getDefaultInputDeviceID
  , getDefaultOutputDeviceID
  , getDeviceInfo
  , openInput
  , openOutput
  , setFilter
  , channel
  , setChannelMask
  , abort
  , close
  , readEvents
  , writeEvents
  , writeShort
  , writeSysEx 
  -- * Time function
  , time
  --
  , encodeMsg
  , decodeMsg
  ) where


import Foreign
import Foreign.C
import Foreign.Marshal
import Foreign.Storable
import Data.IORef
import Data.Bits
import System.IO.Unsafe

import Sound.PortMidi.DeviceInfo

data PMError
  = NoError
  | GotData
  | HostError
  | InvalidDeviceId
  | InsufficientMemory
  | BufferTooSmall
  | BufferOverflow
  | BadPtr
  | BadData
  | InternalError
  | BufferMaxSize
  deriving (Eq, Show)

instance Enum PMError where
  fromEnum NoError = 0
  fromEnum GotData = 1
  fromEnum HostError = -10000
  fromEnum InvalidDeviceId = -9999
  fromEnum InsufficientMemory = -9998
  fromEnum BufferTooSmall = -9997
  fromEnum BufferOverflow = -9996
  fromEnum BadPtr = -9995
  fromEnum BadData = -9994
  fromEnum InternalError = -9993
  fromEnum BufferMaxSize = -9992
  toEnum 0 = NoError
  toEnum 1 = GotData
  toEnum (-10000) = HostError
  toEnum (-9999) = InvalidDeviceId
  toEnum (-9998) = InsufficientMemory
  toEnum (-9997) = BufferTooSmall
  toEnum (-9996) = BufferOverflow
  toEnum (-9995) = BadPtr
  toEnum (-9994) = BadData
  toEnum (-9993) = InternalError
  toEnum (-9992) = BufferMaxSize
  toEnum x = error $ "PortMidi: toEnum out of bound " ++ show x

toPMError :: CInt -> PMError
toPMError = toEnum . fromIntegral

data PortMidiStream
type PMStreamPtr = Ptr PortMidiStream
type PMStream = ForeignPtr PortMidiStream
type DeviceID = Int

(.<.) = shiftL
(.>.) = shiftR

filterActive, filterSysex, filterClock, filterPlay, filterTick, filterFD, filterUndefined, filterReset, filterRealtime, filterNote, filterChannelAftertouch, filterPolyAftertouch, filterAftertouch, filterProgram, filterControl, filterPitchBend, filterMTC, filterSongPosition, filterSongSelect, filterTune, filterSystemCommon :: CLong

filterActive = 1 .<. 0x0e 
filterSysex = 1 .<. 0x00
filterClock = 1 .<. 0x08
filterPlay = (1 .<. 0x0A) .|. (1 .<. 0x0C) .|. (1 .<. 0x0B)
filterTick = 1 .<. 0x09
filterFD = 1 .<. 0x0D
filterUndefined = filterFD
filterReset = 1 .<. 0x0F
filterRealtime = filterActive .|. filterSysex .|. filterClock .|. filterPlay .|. filterUndefined .|. filterReset .|. filterTick 
filterNote = (1 .<. 0x19) .|. (1 .<. 0x18)
filterChannelAftertouch = 1 .<. 0x1D
filterPolyAftertouch = 1 .<. 0x1A
filterAftertouch = filterChannelAftertouch .|. filterPolyAftertouch
filterProgram = 1 .<. 0x1C
filterControl = 1 .<. 0x1B
filterPitchBend = 1 .<. 0x1E
filterMTC = 1 .<. 0x01
filterSongPosition = 1 .<. 0x02
filterSongSelect = 1 .<. 0x03
filterTune = 1 .<. 0x06
filterSystemCommon = filterMTC .|. filterSongPosition .|. filterSongSelect .|. filterTune

data PMMsg 
  =  PMMsg
  { status :: !CLong
  , data1  :: !CLong
  , data2  :: !CLong
  } deriving (Eq, Show)

encodeMsg (PMMsg s d1 d2) = ((d2 .&. 0xFF) .<. 16) .|. ((d1 .&. 0xFF) .<. 8) .|. (s .&. 0xFF)
decodeMsg i = PMMsg (i .&. 0xFF) ((i .>. 8) .&. 0xFF) ((i .>. 16) .&. 0xFF)

type Timestamp = CULong

data PMEvent 
  =  PMEvent 
  { message   :: !PMMsg
  , timestamp :: !Timestamp
  } deriving (Eq, Show)

instance Storable PMEvent where
  sizeOf _ = sizeOf (0::CLong) * 2
  alignment _ = alignment (0::CLong)
  peek ptr = do
    m <- peekByteOff ptr 0
    t <- peekByteOff ptr (sizeOf m)
    return $ PMEvent (decodeMsg m) t
  poke ptr (PMEvent m t) = do
    let v = encodeMsg m :: CLong
    pokeByteOff ptr 0 v 
    pokeByteOff ptr (sizeOf v) t


foreign import ccall "portmidi.h Pm_Initialize" pm_Initialize :: IO CInt
initialize :: IO PMError
initialize = pm_Initialize >>= return . toPMError

foreign import ccall "portmidi.h Pm_Terminate" pm_Terminate :: IO CInt
terminate :: IO PMError
terminate = pm_Terminate >>= return . toPMError

foreign import ccall "portmidi.h Pm_HasHostError" pm_HasHostError :: PMStreamPtr -> IO CInt
hasHostError :: PMStream -> IO Bool
hasHostError = flip withForeignPtr (\stream -> pm_HasHostError stream >>= return . toEnum . fromIntegral)

foreign import ccall "portmidi.h Pm_GetErrorText" pm_GetErrorText :: CInt -> IO CString
getErrorText :: PMError -> IO String
getErrorText err = pm_GetErrorText (fromIntegral $ fromEnum err) >>= peekCString

foreign import ccall "portmidi.h Pm_CountDevices" pm_countDevices :: IO CInt
countDevices :: IO DeviceID
countDevices = pm_countDevices >>= return . fromIntegral

foreign import ccall "portmidi.h Pm_GetDefaultInputDeviceID" pm_GetDefaultInputDeviceID :: IO CInt
getDefaultInputDeviceID :: IO (Maybe DeviceID)
getDefaultInputDeviceID = do
  i <- pm_GetDefaultInputDeviceID
  return $ if i == -1 then Nothing else Just (fromIntegral i)
foreign import ccall "portmidi.h Pm_GetDefaultOutputDeviceID" pm_GetDefaultOutputDeviceID :: IO CInt
getDefaultOutputDeviceID :: IO (Maybe DeviceID)
getDefaultOutputDeviceID = do
  i <- pm_GetDefaultOutputDeviceID
  return $ if i == -1 then Nothing else Just (fromIntegral i)

foreign import ccall "portmidi.h Pm_GetDeviceInfo" pm_GetDeviceInfo :: CInt -> IO (Ptr ())
getDeviceInfo :: DeviceID -> IO DeviceInfo
getDeviceInfo deviceID = pm_GetDeviceInfo (fromIntegral deviceID) >>= peekDeviceInfo

foreign import ccall "portmidi.h Pm_OpenInput" pm_OpenInput :: Ptr PMStreamPtr -> CInt -> Ptr () -> CLong -> Ptr () -> Ptr () -> IO CInt
openInput :: DeviceID -> IO (Either PMStream PMError)
openInput inputDevice = 
  with nullPtr (\ptr -> do
    e <- pm_OpenInput ptr (fromIntegral inputDevice) nullPtr 0 nullPtr nullPtr
    if e == 0 
      then do
        stream <- peek ptr
        ptr' <- newForeignPtr_ stream
        return $ Left ptr'
      else return (Right (toPMError e)))
      
foreign import ccall "portmidi.h Pm_OpenOutput" pm_OpenOutput :: Ptr PMStreamPtr -> CInt -> Ptr () -> CLong -> Ptr () -> Ptr () -> CLong -> IO CInt
openOutput :: DeviceID -> Int -> IO (Either PMStream PMError)
openOutput outputDevice latency =
  with nullPtr (\ptr -> do
    e <- pm_OpenOutput ptr (fromIntegral outputDevice) nullPtr 0 nullPtr nullPtr (fromIntegral latency)
    if e == 0 
      then do
        stream <- peek ptr
        ptr' <- newForeignPtr_ stream
        return $ Left ptr'
      else return (Right (toPMError e)))

foreign import ccall "portmidi.h Pm_SetFilter" pm_SetFilter :: PMStreamPtr -> CLong -> IO CInt
setFilter :: PMStream -> CLong -> IO PMError
setFilter stream filter = withForeignPtr stream (\s -> pm_SetFilter s filter >>= return . toPMError)

channel :: Int -> CLong
channel i = 1 .<. i

foreign import ccall "portmidi.h Pm_SetChannelMask" pm_SetChannelMask :: PMStreamPtr -> CLong -> IO CInt
setChannelMask :: PMStream -> CLong -> IO PMError
setChannelMask stream mask = withForeignPtr stream (\s -> pm_SetChannelMask s mask >>= return . toPMError)

foreign import ccall "portmidi.h Pm_Abort" pm_Abort :: PMStreamPtr -> IO CInt
abort :: PMStream -> IO PMError
abort = flip withForeignPtr (\s -> pm_Abort s >>= return . toPMError)

foreign import ccall "portmidi.h Pm_Close" pm_Close :: PMStreamPtr -> IO CInt
close :: PMStream -> IO PMError
close = flip withForeignPtr (\s -> pm_Close s >>= return . toPMError)

foreign import ccall "portmidi.h Pm_Read" pm_Read :: PMStreamPtr -> Ptr PMEvent -> CLong -> IO CInt
readEvents :: PMStream -> IO (Either [PMEvent] PMError)
readEvents = flip withForeignPtr (\s -> allocaArray (fromIntegral defaultBufferSize) (\arr -> do
  r <- pm_Read s arr defaultBufferSize
  if r > 0
    then peekArray (fromIntegral r) arr >>= return . Left
    else return $ Right (toPMError r)))
  where
    defaultBufferSize = 256

foreign import ccall "portmidi.h Pm_Write" pm_Write :: PMStreamPtr -> Ptr PMEvent -> CLong -> IO CInt
writeEvents :: PMStream -> [PMEvent] -> IO PMError
writeEvents stream events = withForeignPtr stream (\s -> 
  withArrayLen events (\len arr -> pm_Write s arr (fromIntegral len) >>= return . toPMError))

foreign import ccall "portmidi.h Pm_WriteShort" pm_WriteShort :: PMStreamPtr -> CULong -> CLong -> IO CInt
writeShort :: PMStream -> PMEvent -> IO PMError
writeShort stream (PMEvent msg time) = withForeignPtr stream (\s ->
  pm_WriteShort s time (encodeMsg msg) >>= return . toPMError)

foreign import ccall "portmidi.h Pm_WriteSysEx" pm_WriteSysEx :: PMStreamPtr -> CULong -> CString -> IO CInt
writeSysEx :: PMStream -> Timestamp -> String -> IO PMError
writeSysEx stream time str = withForeignPtr stream (\st ->
  withCAString str (\s -> pm_WriteSysEx st time s >>= return . toPMError))

foreign import ccall "porttime.h Pt_Time" time :: IO Timestamp      

