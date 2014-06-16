module TreePatch where

import Data.Tree
import Control.Arrow (first, second)
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

findAndDelete :: (a -> Bool) -> [a] -> (Maybe a,[a])
findAndDelete f (x:xs) = if f x
                         then (Just x,xs)
                         else second (x:) $ findAndDelete f xs
findAndDelete _ [] = (Nothing,[])

findInForest x = findAndDelete ((==x) . rootLabel)

patch :: Operation -> Tree T.Text -> Tree T.Text
patch (Add (x:xs) val) tr@(Node i ts) = case (xs,findInForest x ts) of
  ([],(Just k, tts)) -> Node x $ leaf val : tts
  ([],(Nothing, _))  -> Node x $ leaf val : ts
  (_, (Just n, tts)) -> Node i $ patch (Add xs val) n : tts
  (_, (Nothing, _))  -> tr
patch (Add [] val) (Node i ts) = Node i [leaf val]

patch (Rem (x:xs)) tr@(Node i ts) = case (xs,findInForest x ts) of
  ([],(Just n, tts)) -> Node i tts
  ([],(Nothing, _))  -> tr
  (_, (Just n, tts)) -> Node i $ patch (Rem xs) n : tts
  (_, (Nothing, _))  -> tr
patch (Rem []) (Node i ts) = leaf i

patch _ _ = undefined
