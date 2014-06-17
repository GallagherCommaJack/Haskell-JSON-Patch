module TreePatch where

import Data.Tree (Tree(..), rootLabel)
import Control.Arrow (first, second)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>), mconcat)
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

toPath :: Text -> Path
toPath = T.split (=='/')

fromPath :: Path -> Text
fromPath = mconcat . fmap (flip T.snoc '/')

findAndDeleteAtPath :: Path -> Tree Text -> (Maybe (Tree Text),Tree Text)
findAndDeleteAtPath (x:xs) tr@(Node i ts) = case (xs,findInForest x ts) of
  ([],(Just n, tts)) -> (Just n, Node i tts)
  ([],(Nothing, _))  -> (Nothing, tr)
  (_, (Just n, tts)) -> second (Node i . (:tts)) $ findAndDeleteAtPath xs n
  (_, (Nothing, _))  -> (Nothing, tr)
findAndDeleteAtPath [] tr@(Node i ts) = (Just tr, leaf i)

patch :: Operation -> Tree Text -> Either Text (Tree Text)
patch (Add (x:xs) val) tr@(Node i ts) = case (xs,findInForest x ts) of
  ([],(Just k, tts)) -> Right $ Node x $ leaf val : tts
  ([],(Nothing, _))  -> Right $ Node x $ leaf val : ts
  (_, (Just n, tts)) -> Node i <$> (:tts) <$> patch (Add xs val) n
  (_, (Nothing, _))  -> Right tr
patch (Add [] val) (Node i ts) = Right $ leaf val

patch (Rem []) (Node i ts) = Right $ leaf i
patch (Rem xs) tr = Right $ snd $ findAndDeleteAtPath xs tr

patch (Cop p1 p2) tr = case fst $ findAndDeleteAtPath p1 tr of
  (Just v) -> patch (Add p2 $ rootLabel v) tr
  Nothing  -> Left $ "Could not find value at path " <> fromPath p1

patch (Mov p1 p2) tr = case findAndDeleteAtPath p1 tr of
  (Just v, trr) -> patch (Add p2 $ rootLabel v) trr
  (Nothing, _)  -> Left $ "Could not find value at path " <> fromPath p1

patch (Rep p v) tr = case findAndDeleteAtPath p tr of
  (Just v, trr) -> patch (Add p $ rootLabel v) trr
  (Nothing, _)  -> Left $ "Could not find value at path " <> fromPath p

patch (Tes p t) tr = case fst $ findAndDeleteAtPath p tr of
  (Just (Node i _)) -> if i == t then Right tr else Left $ "Test failed, \n" <>
                                                    i <> " \\= " <> t
  Nothing -> Left $ "Test failed, couldn't find value at " <> fromPath p
