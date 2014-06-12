module ParsePatch
       (Patch(..),
        parsePatch
       )
       where

import Data.Maybe (fromJust)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Encoding    as T

import Data.Aeson

data Patch = Patch { op :: T.Text,
                     path :: T.Text,
                     from :: Maybe T.Text,
                     value :: Maybe T.Text
                   } deriving (Show, Read, Eq)

--
instance FromJSON Patch where
  parseJSON (Object v) = Patch <$>
                         v .: "op" <*>
                         v .: "path" <*>
                         v .:? "from" <*>
                         v .:? "value"
  parseJSON _ = mzero

--
instance ToJSON Patch where
  toJSON p@(Patch op path from value) = case op of
    "add" -> case value of
      (Just val) -> object [ "op" .= op,
                             "path" .= path,
                             "value" .= val]
      Nothing -> error $ "Cannot convert " ++ show p ++ " to JSON"
    "remove" -> object [ "op" .= op,
                         "path" .= path ]
    _ -> error $ "Cannot convert " ++ show p ++ " to JSON"
--}
jsonTest :: BS.ByteString
jsonTest = BS.pack $ "{ \"op\" : \"add\", \"path\" : \"/derp\"" ++
                     ", \"value\" : \"derp\" }"

parsePatch :: T.Text -> Maybe Patch
parsePatch = decode . T.encodeUtf8
--
patchFile :: String -> IO [Patch]
patchFile file = fromJust <$> decode <$> BS.readFile file
--}
