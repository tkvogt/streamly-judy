import qualified Data.Judy as J
import Data.JudyStreaming

main :: IO ()
main = do
    j <- J.new
    J.insert 1234 567 j
    encodeJudy j "judyArray"
    dj <- decodeJudy "judyArray"
    fdj <- J.freeze dj
    l <- J.toList fdj
    putStrLn (show l)

