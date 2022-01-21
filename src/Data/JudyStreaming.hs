{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
module Data.JudyStreaming
    ( encodeJudy
    , decodeJudy
    , decodeJudy2
    , streamJudy
    , wordToWord8
    , word32ToWord8
    , extractWord8
    ) where

#if !defined(UNSAFE)
import Control.Concurrent
#endif
import qualified Data.ByteString as B
import Data.Function((&))
import Data.Judy(JE(..), JudyL(..))
import qualified Data.Judy as J
import Data.Maybe(isJust, fromJust)
import qualified Streamly.Prelude as Stream
import Foreign hiding (new)
import Foreign.C.Types
import Foreign.ForeignPtr()
import GHC.Ptr
import GHC.Word(Word(..))
import Network.ByteOrder(word32,word64)
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.Data.Array.Foreign as Foreign
import           System.IO.Unsafe(unsafePerformIO)


-- | Store a judy array into a file
encodeJudy :: JE a => J.JudyL a -> FilePath -> IO ()
encodeJudy j target = do
    streamJudy j
      & Stream.mapM encodeKeyValue
      & Stream.map Foreign.fromList
      & File.fromChunks target
  where encodeKeyValue :: JE a => (J.Key,a) -> IO [Word8]
        encodeKeyValue (key,value) = do w <- toWord value
                                        return ((wordToWord8 key) ++ (word32ToWord8 w))


-- | Load a judy array from file
decodeJudy :: JE a => FilePath -> IO (J.JudyL a)
decodeJudy file =
    File.toChunks file
  & Stream.take 12
  & Stream.map (readLine . B.pack . Foreign.toList)
  & Stream.foldlM' ins J.new
 where ins :: JE a => J.JudyL a -> (Word64, Word) -> IO (J.JudyL a)
       ins j (key,value) = do v <- fromWord value
                              J.insert (fromIntegral key) v j
                              return j
       readLine :: B.ByteString -> (Word64, Word)
       readLine word8s = (key, value)
         where key = word64 (B.take 8 word8s)
               value = fromIntegral (word32 (B.take 4 (B.drop 8 word8s)))


-- | Load a judy array from a bytestring
decodeJudy2 :: JE a => B.ByteString -> IO (J.JudyL a)
decodeJudy2 bs = do n <- J.new
                    toJudy bs n
  where toJudy :: JE a => B.ByteString -> J.JudyL a -> IO (J.JudyL a)
        toJudy b j | B.null b  = return j
                   | otherwise = do v <- fromWord value
                                    J.insert (fromIntegral key) v j
                                    toJudy (B.drop 12 b) j
          where key = word64 (B.take 8 b)
                value = fromIntegral (word32 (B.take 4 (B.drop 8 b)))


-----------------------------------------------------------------------------------------
-- Helpers
-- | Converting a judy array to a list (with J.toList j) is problematic when the array 
--   is big an needs a lot of memory. It is more reasonable to stream the judy array when 
--   (for example) writing to disk
streamJudy :: JE a => J.JudyL a -> Stream.SerialT IO (J.Key,a)
streamJudy j = Stream.iterateM next initial
                 & Stream.takeWhile isJust
                 & Stream.map fromJust
  where
    initial :: JE a => IO (Maybe (J.Key,a))
    initial =
#if !defined(UNSAFE)
      withMVar (unJudyL j) $ \m_ ->
        withForeignPtr m_ $ \p -> do
#else
        withForeignPtr (unJudyL m)  $ \p -> do
#endif
          q <- peek p -- get the actual judy array
          alloca $ \k_ptr -> do
            poke k_ptr 0
            v_ptr <- J.c_judy_lfirst q k_ptr J.nullError
            if v_ptr == nullPtr
               then return Nothing
               else do k <- peek k_ptr
                       v <- fromWord =<< peek v_ptr
                       return (Just (k,v))

    next :: JE a => (Maybe (J.Key,a)) -> IO (Maybe (J.Key,a))
    next r = case r of
      Nothing -> return Nothing
      Just (k0,v0)  -> do
#if !defined(UNSAFE)
        withMVar (unJudyL j) $ \m_ ->
          withForeignPtr m_ $ \p -> do
#else
          withForeignPtr (unJudyL m)  $ \p -> do
#endif
            q <- peek p -- get the actual judy array
            -- dellocate
            alloca $ \k_ptr -> do
                poke k_ptr k0
                v_ptr <- J.c_judy_lnext q k_ptr J.nullError
                if v_ptr == nullPtr
                   then return Nothing
                   else do
                       k <- peek k_ptr
                       v <- fromWord =<< peek v_ptr
                       return (Just (k,v))

wordToWord8 :: Word -> [Word8]
wordToWord8 w = map (extractWord8 w) [7,6,5,4,3,2,1,0]

word32ToWord8 :: Word -> [Word8]
word32ToWord8 w = map (extractWord8 w) [3,2,1,0]

extractWord8 :: Word -> Int -> Word8
extractWord8 w i
    = unsafePerformIO . allocaBytes 1 $ \p -> do
        pokeByteOff p 0 w
        peek (castPtr (plusPtr p i))

