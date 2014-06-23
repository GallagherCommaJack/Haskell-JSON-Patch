module ValuePatch (patch
                  ,applyPatches
                  )
       where


import Data.Aeson (Value(..))

import Control.Applicative ((<$>))
import Data.Monoid ((<>), mconcat)

import Data.Text (Text(..))

import qualified Data.Text as T (pack, snoc)

import Data.HashMap.Strict (HashMap(..))
import qualified Data.HashMap.Strict as HM (lookup, insert, delete, adjust)

import ParsePatch (Operation(..))

type Path = [Text]

objToText :: Value -> Text
objToText (Object o) = T.pack $ show o
objToText (Array v) = T.pack $ show v
objToText (Number n) = T.pack $ show n
objToText (Bool b) = T.pack $ show b
objToText (String t) = t
objToText Null = ""

findAndDelete :: Path -> Value -> Maybe (Value,Value)
findAndDelete [k] (Object h) =
  HM.lookup k h >>= \v -> Just (v,Object $ HM.delete k h)
findAndDelete (x:xs) (Object h) = do
  sub <- HM.lookup x h
  (v,new) <- findAndDelete xs sub
  let deleted = HM.delete x h
  return (v, Object $ HM.insert x new deleted)
findAndDelete _ _ = Nothing

--Wouldn't be necessary but Aeson uses strict hashmaps...
findAtPath :: Path -> Value -> Maybe Value
findAtPath (k:ks) (Object h) = HM.lookup k h >>= findAtPath ks
findAtPath [] a = Just a

addAtPath :: Path -> Value -> Value -> Maybe Value
addAtPath (p:ps) v obj = do
  (sub, (Object rest)) <- findAndDelete [p] obj
  patched <- addAtPath ps v sub
  return $ Object $ HM.insert p patched rest
addAtPath [] v _ = Just v

fromPath :: Path -> Text
fromPath = mconcat . fmap (flip T.snoc '/')

maybeToEither :: Maybe a -> Either Text a
maybeToEither (Just a) = Right a
maybeToEither Nothing = Left "Nothing"

patch :: Operation -> Value -> Either Text Value
patch (Add p v) obj = case addAtPath p v obj of
  Just a -> Right a
  Nothing -> Left $ "Couldn't traverse path " <> fromPath p

patch (Rem p) obj = case snd <$> findAndDelete p obj of
  (Just a) -> Right a
  Nothing -> Left $ "Nothing to remove at " <> fromPath p

patch (Cop p1 p2) obj = case findAtPath p1 obj of
  (Just old) -> patch (Add p2 old) obj
  Nothing   -> Left $ "Couldn't find value at " <> fromPath p1

patch (Mov p1 p2) obj = do
  (old,new) <- maybeToEither $ findAndDelete p1 obj
  patch (Add p2 old) new

patch (Rep p v) obj = case findAndDelete p obj of
  Nothing -> Left $ "Couldn't find value at path " <> fromPath p
  _ -> patch (Add p v) obj

patch (Tes p t) obj = case findAtPath p obj of
  (Just v) ->  if t == v || objToText t == objToText v
               then return obj
               else Left $ "Value at path " <> fromPath p <> " is "
                    <> objToText v <> " not " <> objToText t
  Nothing -> Left $ "Couldn't find value at path " <> fromPath p
