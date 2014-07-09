module ParsePatch (Operation(..), Ix(..), parsePatchFile, parsePatches)
       where

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Text (Text(..))
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString.Lazy.Char8 as BS

data Patch = Patch { op :: Text,
                     path :: Text,
                     from :: Maybe Text,
                     value :: Maybe Value
                   }

instance FromJSON Patch where
  parseJSON (Object v) = Patch <$>
                         v .: "op" <*>
                         v .: "path" <*>
                         v .:? "from" <*>
                         v .:? "value"
  parseJSON _ = mzero

-- |Parses a file of patches, throws an error if it can't parse
parsePatchFile :: String -> IO [Operation]
parsePatchFile file = do
  parsed <- decode <$> BS.readFile file
  let ops = join $ sequence <$> map patchToOp <$> parsed
  case ops of
    (Just xs) -> return xs
    Nothing -> error $ "File " <> file <> " contains invalid patches"

-- |Parses any given ByteString, either returns a list of Operations or a String error message
parsePatches :: BS.ByteString -> Either String [Operation]
parsePatches str = case decode str >>= sequence . map patchToOp of
  (Just p) -> Right p
  Nothing -> Left $ "Can't parse string: " <> BS.unpack str

toPath :: Text -> [Ix]
toPath "" = [K ""]
toPath "/" = []
toPath ps | T.findIndex (=='/') ps == Just 0 = map tToIx $ tail $ T.split (=='/') ps
          | otherwise = map tToIx $ T.split (=='/') ps

tToIx t | T.all isDigit t && not (T.null t) = N $ read $ T.unpack t
        -- Handle forward slash literals
        -- http://tools.ietf.org/html/rfc6901
        | otherwise = K (T.replace "~0" "~" (T.replace "~1" "/" t))

-- |Datatype for indexing through JSON values
data Ix = N Int | K Text

instance Show Ix where
  show (N n) = show n
  show (K t) = T.unpack t

-- |Datatype to store Operations
data Operation = Add [Ix] Value
               | Rem [Ix]
               | Cop [Ix] [Ix]
               | Mov [Ix] [Ix]
               | Rep [Ix] Value
               | Tes [Ix] Value
               deriving (Show)

patchToOp p = case op p of
  "add"     -> Add (toPath $ path p) <$> value p
  "replace" -> Rep (toPath $ path p) <$> value p
  "test"    -> Tes (toPath $ path p) <$> value p
  "copy"    -> flip Cop (toPath $ path p) <$> toPath <$> from p
  "move"    -> flip Mov (toPath $ path p) <$> toPath <$> from p
  "remove"  -> Just $ Rem $ toPath $ path p
  _ -> Nothing
