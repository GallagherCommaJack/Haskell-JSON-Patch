module ValuePatch (patch
                  ,applyPatches
                  )
       where

import Data.List (foldl')

import Data.Aeson (Value(..))

import Control.Applicative ((<$>))
import Data.Monoid ((<>), mconcat)

import Data.Text (Text(..))
import qualified Data.Text as T (all, unpack, pack, snoc)

import Data.Char (isDigit)

import Data.HashMap.Strict (HashMap(..))
import qualified Data.HashMap.Strict as HM (lookup, insert, delete, adjust)

import qualified Data.Vector as V
import Data.Vector.Mutable (write)

import ParsePatch

type Path = [Ix]

objToText :: Value -> Text
objToText (Object o) = T.pack $ show o
objToText (Array v) = T.pack $ show v
objToText (Number n) = T.pack $ show n
objToText (Bool b) = T.pack $ show b
objToText (String t) = t
objToText Null = ""

findAndDelete :: Path -> Value -> Maybe (Value,Value)
findAndDelete [(K k)] (Object h) = HM.lookup k h >>= \v -> Just (v,Object $ HM.delete k h)
findAndDelete ((K k):xs) (Object h) = do sub <- HM.lookup k h
                                         (v,new) <- findAndDelete xs sub
                                         let deleted = HM.delete k h
                                         return (v, Object $ HM.insert k new deleted)
findAndDelete [(N i)] (Array arr) =
  let (first, rest) = V.splitAt i arr
      (el, rest') = V.splitAt 1 rest
  in do e <- el V.!? 0
        return (e, Array $ first <> rest')
findAndDelete ((N i):ps) (Array arr) =
  let (first, rest) = V.splitAt i arr
      (el, rest') = V.splitAt 1 rest
  in do e <- el V.!? 0
        (v, rest) <- findAndDelete ps e
        return (v, Array $ V.modify (\a -> write a i rest) arr)
findAndDelete _ _ = Nothing

--Wouldn't be necessary but Aeson uses strict hashmaps...
findAtPath :: Path -> Value -> Maybe Value
findAtPath ((K k):ks) (Object h) = HM.lookup k h >>= findAtPath ks
findAtPath ((N i):ps) (Array a)  = a V.!? i >>= findAtPath ps
findAtPath [] a = Just a

addAtPath :: Path -> Value -> Value -> Maybe Value
addAtPath [(K k)] v (Object h) = Just $ Object $ HM.insert k v h
addAtPath [(N i)] v (Array arr) = Just $ Array $ V.modify (\vec -> write vec i v) arr
addAtPath (p:ps) v obj = do (sub, o) <- findAndDelete [p] obj
                            case o of
                              (Object rest) -> do patched <- addAtPath ps v sub
                                                  return $ Object $ HM.insert k patched rest
                              (Array rest) -> do patched <- addAtPath ps v sub
                                                 return $ Array $ V.modify (\vec -> write vec i v) rest
  where i :: Int
        i = case p of (N i) -> i
                      _ -> undefined
        k = case p of (K k') -> k'
                      _ -> undefined
addAtPath [] v _ = Just v

maybeToEither :: Maybe a -> Either Text a
maybeToEither (Just a) = Right a
maybeToEither Nothing = Left "Nothing"

patch :: Operation -> Value -> Either Text Value
patch (Add [] v)  _   = Right v
patch (Rem [])    _   = Left "Tried to delete whole file - use rm instead"
patch (Cop [] p2) obj = patch (Add p2 obj) obj
patch (Mov [] _)  _   = Left "Can't move whole document to subdirectory of document!"
patch (Rep [] v)  _   = Right v
patch (Tes [] v)  obj = if v == obj
                        then Right obj
                        else Left $ "Value at path / is "
                             <> objToText obj <> " not " <> objToText v

patch (Add p v) obj = case addAtPath p v obj of
  Just a -> Right a
  Nothing -> Left $ "Couldn't traverse path " <> fromPath p

patch (Rem p) obj = case snd <$> findAndDelete p obj of
  (Just a) -> Right a
  Nothing -> Left $ "Nothing to remove at " <> fromPath p

patch (Cop p1 p2) obj = case findAtPath p1 obj of
  (Just old) -> patch (Add p2 old) obj
  Nothing   -> Left $ "Couldn't find value at " <> fromPath p1

patch (Mov p1 p2) obj = do (old,new) <- maybeToEither $ findAndDelete p1 obj
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

applyPatches :: [Operation] -> Value -> Either Text Value
applyPatches ops v = foldl' (>>=) (Right v) $ map patch ops
