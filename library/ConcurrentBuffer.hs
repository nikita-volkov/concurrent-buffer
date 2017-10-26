module ConcurrentBuffer
(
  Buffer,
  new,
  push,
  pushBytes,
  pushStorable,
  pull,
  pullBytes,
  pullStorable,
  getSpace,
  getBytes,
)
where

import ConcurrentBuffer.Prelude hiding (State, Buffer)
import qualified ConcurrentBuffer.PtrIO as A
import qualified Data.ByteString.Internal as C


data Buffer =
  {-|
  * Buffer pointer
  * Start offset
  * End offset
  * Capacity
  -}
  Buffer
    {-# UNPACK #-} !(TVar (ForeignPtr Word8))
    {-# UNPACK #-} !(TVar Int)
    {-# UNPACK #-} !(TVar Int)
    {-# UNPACK #-} !(TVar Int)
    {-# UNPACK #-} !(TVar Bool)
    {-# UNPACK #-} !(TVar Bool)
    {-# UNPACK #-} !(TVar Bool)


{-|
Create a new buffer of the specified initial capacity.
-}
new :: Int -> IO Buffer
new capacity =
  do
    fptr <- mallocForeignPtrBytes capacity
    atomically $ do
      fptrVar <- newTVar fptr
      startVar <- newTVar 0
      endVar <- newTVar 0
      capVar <- newTVar capacity
      notPullingVar <- newTVar True
      notPushingVar <- newTVar True
      notAligningVar <- newTVar True
      return (Buffer fptrVar startVar endVar capVar notPullingVar notPushingVar notAligningVar)

{-|
Prepares the buffer to be filled with at maximum the specified amount of bytes,
then uses the pointer-action to populate it.
It is your responsibility to ensure that the action does not exceed the space limit.

The pointer-action returns the amount of bytes it actually writes to the buffer.
That amount then is used to move the buffer's cursor accordingly.
It can also produce some @result@, which will then be emitted by @push@.

It also aligns or grows the buffer if required.
-}
push :: Buffer -> Int -> (Ptr Word8 -> IO (Int, result)) -> IO result
push (Buffer fptrVar startVar endVar capVar notPullingVar notPushingVar notAligningVar) space ptrIO =
  join $ atomically $ do
    notPushing <- readTVar notPushingVar
    guard notPushing
    writeTVar notPushingVar False
    fptr <- readTVar fptrVar
    start <- readTVar startVar
    end <- readTVar endVar
    capacity <- readTVar capVar
    let
      !remainingSpace = capacity - end
      !capacityDelta = space - remainingSpace
      !occupiedSpace = end - start
    if capacityDelta <= 0 -- Doesn't need more space?
      then
        return $ do
          (!pushedSpace, output) <- withForeignPtr fptr $ \ptr -> ptrIO (plusPtr ptr end)
          atomically $ do
            writeTVar endVar $! end + pushedSpace
            writeTVar notPushingVar True
          return output
      else if capacityDelta > start -- Needs growing?
        then
          -- Grow
          return $ do
            let !newCapacity = occupiedSpace + space
            newFPtr <- mallocForeignPtrBytes newCapacity
            (!pushedSpace, output) <- withForeignPtr newFPtr $ \newPtr -> do
              withForeignPtr fptr $ \ptr -> A.memcpy newPtr (plusPtr ptr start) (fromIntegral occupiedSpace)
              ptrIO (plusPtr newPtr occupiedSpace)
            let !newEnd = occupiedSpace + pushedSpace
            atomically $ do
              divergedStart <- readTVar startVar
              let !newStart = divergedStart - start
              writeTVar fptrVar newFPtr
              writeTVar startVar newStart
              writeTVar endVar newEnd
              writeTVar capVar newCapacity
              writeTVar notPushingVar True
            return output
        else
          -- Align
          do
            notPulling <- readTVar notPullingVar
            guard notPulling
            writeTVar notAligningVar False
            return $ do
              (!pushedSpace, output) <- withForeignPtr fptr $ \ptr -> do
                A.memmove ptr (plusPtr ptr start) (fromIntegral occupiedSpace)
                ptrIO (plusPtr ptr occupiedSpace)
              atomically $ do
                writeTVar startVar 0
                writeTVar endVar $! occupiedSpace + pushedSpace
                writeTVar notAligningVar True
                writeTVar notPushingVar True
              return output

{-|
Pulls the specified amount of bytes from the buffer using the provided pointer-action,
freeing the buffer from the pulled bytes afterwards.

In case the buffer does not contain enough bytes yet, it will block waiting.
-}
pull :: Buffer -> Int -> (Ptr Word8 -> IO result) -> IO result
pull (Buffer fptrVar startVar endVar capVar notPullingVar notPushingVar notAligningVar) amount ptrIO =
  join $ atomically $ do
    notPulling <- readTVar notPullingVar
    guard notPulling
    fptr <- readTVar fptrVar
    start <- readTVar startVar
    end <- readTVar endVar
    guard (amount <= end - start)
    notAligning <- readTVar notAligningVar
    guard notAligning
    writeTVar notPullingVar False
    return $ do
      pulled <- withForeignPtr fptr $ \ptr -> ptrIO (plusPtr ptr start)
      atomically $ do
        start <- readTVar startVar
        writeTVar startVar $! start + amount
        writeTVar notPullingVar True
      return pulled

{-|
Push a byte array into the buffer.
-}
{-# INLINE pushBytes #-}
pushBytes :: Buffer -> ByteString -> IO ()
pushBytes buffer (C.PS bytesFPtr offset length) =
  push buffer length $ \ptr ->
  withForeignPtr bytesFPtr $ \bytesPtr ->
  C.memcpy ptr (plusPtr bytesPtr offset) length $> (length, ())

{-|
Pulls the specified amount of bytes.
-}
{-# INLINE pullBytes #-}
pullBytes :: Buffer -> Int -> IO ByteString
pullBytes buffer amount =
  pull buffer amount (\ptr -> C.create amount (\destPtr -> C.memcpy destPtr ptr amount))

{-|
Push a storable value into the buffer.
-}
{-# INLINE pushStorable #-}
pushStorable :: (Storable storable) => Buffer -> storable -> IO ()
pushStorable buffer storable =
  push buffer amount (\ptr -> poke (castPtr ptr) storable $> (amount, ()))
  where
    amount = sizeOf storable

{-|
Pulls a storable value.
-}
{-# INLINE pullStorable #-}
pullStorable :: (Storable storable) => Buffer -> IO storable
pullStorable buffer =
  result
  where
    result =
      pull buffer amount (\ptr -> peek (castPtr ptr))
      where
        amount =
          sizeOf ((undefined :: IO a -> a) result)

{-|
Get how much space is occupied by the buffer's data.
-}
{-# INLINE getSpace #-}
getSpace :: Buffer -> IO Int
getSpace (Buffer fptrVar startVar endVar capVar notPullingVar notPushingVar notAligningVar) =
  atomically $ do
    end <- readTVar endVar
    start <- readTVar startVar
    return $! end - start

{-|
Create a bytestring representation without modifying the buffer.
-}
{-# INLINE getBytes #-}
getBytes :: Buffer -> IO ByteString
getBytes (Buffer fptrVar startVar endVar capVar notPullingVar notPushingVar notAligningVar) =
  join $ atomically $ do
    fptr <- readTVar fptrVar
    end <- readTVar endVar
    start <- readTVar startVar
    let size = end - start
    return $ withForeignPtr fptr $ \ptr -> C.create size $ \destPtr -> C.memcpy destPtr ptr size
