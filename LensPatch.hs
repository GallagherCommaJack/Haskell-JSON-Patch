module LensPatch (patch, applyPatches, findAtPath, fAtPath, addAtPath, setAtPath)
       where

import Data.Aeson
import Data.Aeson.Lens

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Lens ((^?), (.~), (%~))

import Data.List (foldl')

import Data.Text (Text(..))
import qualified Data.Text as T (all, unpack, pack, snoc)

import Data.Maybe (fromJust)
import Data.Monoid ((<>))

import qualified Data.Vector as V ((!?), ifilter, modify)
import Data.Vector.Mutable (write)

import qualified Data.HashMap.Strict as HM

import ParsePatch (Ix(..), Operation(..))

findAtPath :: [Ix] -> Value -> Maybe Value
findAtPath ((N n):ps) a = findAtPath ps =<< a ^? nth n
findAtPath ((K k):ps) o = findAtPath ps =<< o ^? key k
findAtPath [] o = Just o

fAtPath :: (Value -> Value) -> [Ix] -> Value -> Value
fAtPath f ((N n):ps) a = nth n %~ fAtPath f ps $ a
fAtPath f ((K k):ps) o = key k %~ fAtPath f ps $ o
fAtPath f [] v = f v

setAtPath :: Value -> [Ix] -> Value -> Value
setAtPath v = fAtPath $ const v

addAtPath :: [Ix] -> Value -> Value -> Value
addAtPath [(N n)] v a = nth n .~ v $ a
addAtPath [(K k)] v o = key k .~ v $ o
addAtPath ((N n):ps) v a = nth n %~ addAtPath ps v $ a
addAtPath ((K k):ps) v o = key k %~ addAtPath ps v $ o
addAtPath [] v _ = v

findAndDelete :: [Ix] -> Value -> Maybe (Value,Value)
findAndDelete [(N n)] (Array v) = do
  el <- v V.!? n
  Just $ (el, Array $ V.ifilter (const . (/= n)) v)
findAndDelete ((N i):ps) (Array v) = do
  sub <- v V.!? i
  (el,sub') <- findAndDelete ps sub
  Just $ (el, Array $ V.modify (\vec -> write vec i sub') v)
findAndDelete [(K k)] (Object o) = do
  el <- HM.lookup k o
  Just $ (el, Object $ HM.delete k o)
findAndDelete ((K k):ps) (Object o) = do
  sub <- HM.lookup k o
  (el,sub') <- findAndDelete ps sub
  Just $ (el, Object $ HM.insert k sub' o)
findAndDelete _ _ = Nothing

patch :: Operation -> Value -> Either Text Value
patch (Add [] v)  _   = Right v
patch (Rep [] v)  _   = Right v
patch (Rem [])    _   = Left "Tried to delete whole file - use rm instead"
patch (Cop [] p2) obj = patch (Add p2 obj) obj
patch (Tes [] v)  obj = if v == obj
                        then Right obj
                        else Left $ "Value at path / is "
                             <> objToText obj <> " not " <> objToText v

patch (Add p v) obj = Right $ addAtPath p v obj
patch (Rem p) obj = case snd <$> findAndDelete p obj of
  (Just a) -> Right a
  Nothing -> Left $ "Nothing to remove at " <> fromPath p
patch (Cop p1 p2) obj = case findAtPath p1 obj of
  (Just old) -> patch (Add p2 old) obj
  Nothing  -> Left $ "Couldn't find value at " <> fromPath p1
patch (Mov p1 p2) obj = case findAndDelete p1 obj of
  (Just (old,new)) -> patch (Add p2 old) new
  Nothing -> Left $  "Can't find value at path " <> fromPath p1
patch (Rep p v) obj = Right $ setAtPath v p obj
patch (Tes p t) obj = case findAtPath p obj of
  (Just v) -> if t == v || objToText t == objToText v
              then return obj
              else Left $ "Value at path " <> fromPath p <> " is "
                   <> objToText v <> " not " <> objToText t
  Nothing -> Left $ "Couldn't find value at path " <> fromPath p

applyPatches :: [Operation] -> Value -> Either Text Value
applyPatches ops v = foldl' (>>=) (Right v) $ map patch ops

objToText :: Value -> Text
objToText (Object o) = T.pack $ show o
objToText (Array v) = T.pack $ show v
objToText (Number n) = T.pack $ show n
objToText (Bool b) = T.pack $ show b
objToText (String t) = t
objToText Null = ""

fromPath :: Path -> Text
fromPath = mconcat . fmap (flip T.snoc '/' . toText)

toText (K k) = k
toText (N i) = T.pack $ show i
