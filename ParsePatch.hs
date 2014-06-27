module ParsePatch (Operation(..),
                   parsePatchFile
                  )
       where

import Data.Monoid ((<>))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, join)

import Data.Text (Text(..))
import qualified Data.Text          as T (split, tail, findIndex)
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

type Path = [Text]

toPath :: Text -> Path
toPath "" = [""]
toPath t | T.findIndex (=='/') t == Just 0  = T.split (=='/') $ T.tail t
         | otherwise = T.split (=='/') t

data Operation = Add Path Value
               | Rem Path
               | Cop Path Path
               | Mov Path Path
               | Rep Path Value
               | Tes Path Value
               deriving (Show)

patchToOp p = case op p of
  "add"     -> value p >>= return . Add (toPath $ path p)
  "replace" -> value p >>= return . Rep (toPath $ path p)
  "test"    -> value p >>= return . Tes (toPath $ path p)
  "copy"    -> from p >>= return . flip Cop (tail $ toPath $ path p) . toPath
  "move"    -> from p >>= return . flip Mov (tail $ toPath $ path p) . toPath
  "remove"  -> Just $ Rem $ toPath $ path p
