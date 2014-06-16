module TreePatch where

import Data.Tree
import Control.Applicative ((<$>), (<*>))
import Data.Text.Lazy (Text(..), unpack)
import qualified Data.Text.Lazy as T hiding (copy, replace)

import ParseTree

type Path = [Text]

data Operation = Add Path Text
               | Rem Path
               | Cop Path Path
               | Mov Path Path
               | Rep Path Text
               | Tes Path Text
               deriving (Show, Read, Eq)
