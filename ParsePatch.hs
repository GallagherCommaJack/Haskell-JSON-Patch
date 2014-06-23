module ParsePatch (Operation(..),
                   parsePatchFile
                  )
       where

import Data.Monoid ((<>))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, join)

import Data.Text (Text(..))
import qualified Data.Text          as T (split)
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

