import Data.Aeson (eitherDecode, encode, Value(..))
import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Text (unpack)

import System.Environment (getArgs)

import Data.ByteString.Lazy.Char8 as BS (readFile, putStrLn, ByteString(..))

import LensPatch (applyPatches)
import ParsePatch (parsePatchFile)

main = do
  args <- getArgs
  case args of
    (p:fs:_) -> do
      ops <- parsePatchFile p
      obj <- eitherDecode <$> BS.readFile fs :: IO (Either String Value)
      case obj of (Right o) ->
                    case applyPatches ops o of
                      (Right n) -> BS.putStrLn $ encode n
                      (Left err) -> error $ unpack err
                  (Left err) -> error $ "Couldn't parse file " <> fs
                                <> ", Aeson threw error:\n" <> err
    _ -> error "Wrong number of arguments"
