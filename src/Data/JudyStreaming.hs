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

type Judy = J.JudyL Word32


-- | Store a judy array into a file
encodeJudy :: Judy -> FilePath -> IO ()
encodeJudy j target = do
    fr <- J.freeze j
    streamJudy fr
      & Stream.map encodeKeyValue
      & Stream.map Foreign.fromList
      & File.fromChunks target
  where encodeKeyValue (key,value) = (wordToWord8 key) ++ (word32ToWord8 (fromIntegral value))


-- | Load a judy array from file
decodeJudy :: FilePath -> IO Judy
decodeJudy file =
    File.toChunks file
  & Stream.take 12
  & Stream.map (readLine . B.pack . Foreign.toList)
  & Stream.foldlM' ins J.new
 where ins :: Judy -> (Word64, Word32) -> IO Judy
       ins j (key,value) = do J.insert (fromIntegral key) value j
                              return j
       readLine :: B.ByteString -> (Word64, Word32)
       readLine word8s = (key, value)
         where key = word64 (B.take 8 word8s)
               value = word32 (B.take 4 (B.drop 8 word8s))


-- | Load a judy array from a bytestring
decodeJudy2 :: B.ByteString -> IO Judy
decodeJudy2 bs = do n <- J.new
                    toJudy bs n
  where toJudy :: B.ByteString -> Judy -> IO Judy
        toJudy b j | B.null b  = return j
                   | otherwise = do J.insert (fromIntegral key) value j
                                    toJudy (B.drop 12 b) j
          where key = word64 (B.take 8 b)
                value = word32 (B.take 4 (B.drop 8 b))


-----------------------------------------------------------------------------------------
-- Helpers
-- | Converting a judy array to a list (with J.toList j) is problematic when the array 
--   is big an needs a lot of memory. It is more reasonable to stream the judy array when 
--   (for example) writing to disk
streamJudy :: JE a => J.JudyImmutable a -> Stream.SerialT IO (J.Key,a)
streamJudy (J.JudyImmutable m) = Stream.iterateM next initial
                                & Stream.takeWhile isJust
                                & Stream.map fromJust
  where
    initial :: JE a => IO (Maybe (J.Key,a))
    initial =
#if !defined(UNSAFE)
      withMVar (unJudyL m) $ \m_ ->
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
        withMVar (unJudyL m) $ \m_ ->
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

