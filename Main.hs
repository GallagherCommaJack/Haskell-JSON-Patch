import Data.Aeson
import Control.Applicative
import Data.List
import Data.Monoid

import System.Environment

import Data.ByteString.Lazy.Char8 as BS hiding (foldl')

import LensPatch
import ParsePatch

main = do
  args <- getArgs
  case args of
    (p:fs:_) -> do
      ops <- parsePatchFile p
      obj <- eitherDecode <$> BS.readFile fs :: IO (Either String Value)
      case foldl' (\v p -> v >>= patch p) obj ops of
        (Right n) -> BS.putStrLn $ encode n
        (Left err) -> error err
    _ -> error "Wrong number of arguments"
