module ParsePatch (Operation(..), Ix(..), parsePatchFile)
       where

import Data.Monoid ((<>))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, join)

import Data.Char (isDigit)

import Data.Text (Text(..))
import qualified Data.Text          as T (split, tail, findIndex, unpack, all)
import qualified Data.Text.Encoding as T (encodeUtf8)

import qualified Data.ByteString.Lazy.Char8 as BS (ByteString(..), pack, readFile)

import Data.Aeson (Value(..), FromJSON(..), (.:), (.:?), decode)

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


parsePatchFile :: String -> IO [Operation]
parsePatchFile file = do
  parsed <- decode <$> BS.readFile file
  let ops = join $ sequence <$> map patchToOp <$> parsed
  case ops of
    (Just xs) -> return xs
    Nothing -> error $ "File " <> file <> " contains invalid patches"
--Conversion to operations

toPath :: Text -> [Ix]
toPath "" = [K ""]
toPath "/" = []
toPath ps | T.findIndex (=='/') ps == Just 0 = map tToIx $ T.split (=='/') ps
          | otherwise = map tToIx $ T.split (=='/') ps

tToIx t | T.all isDigit t = N $ read $ T.unpack t
        | otherwise = K t

data Ix = N Int | K Text

instance Show Ix where
  show (N n) = show n
  show (K t) = T.unpack t

data Operation = Add [Ix] Value
               | Rem [Ix]
               | Cop [Ix] [Ix]
               | Mov [Ix] [Ix]
               | Rep [Ix] Value
               | Tes [Ix] Value
               deriving (Show)

patchToOp p = case op p of
  "add"     -> value p >>= return . Add (toPath $ path p)
  "replace" -> value p >>= return . Rep (toPath $ path p)
  "test"    -> value p >>= return . Tes (toPath $ path p)
  "copy"    -> from p >>= return . flip Cop (tail $ toPath $ path p) . toPath
  "move"    -> from p >>= return . flip Mov (tail $ toPath $ path p) . toPath
  "remove"  -> Just $ Rem $ toPath $ path p
