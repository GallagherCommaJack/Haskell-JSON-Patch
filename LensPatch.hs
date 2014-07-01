{-# LANGUAGE RankNTypes #-}
module LensPatch (patch,
                  applyPatches,
                  findAtPath,
                  fAtPath,
                  fAtPathA,
                  addAtPath,
                  addAtPathA,
                  setAtPath,
                  setAtPathA,
                  toLens,
                  setj,
                  remove
                 ) where

import Prelude hiding (foldr1)
import Data.Aeson
import Data.Aeson.Lens

import Data.Foldable (Foldable(..), foldr1)

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Arrow ((***))
import Control.Exception (assert)
import Control.Lens

import Data.Monoid ((<>))
import Data.Vector (ifilter)
import Data.HashMap.Strict (delete)
import ParsePatch (Ix(..), Operation(..))

toLens :: (AsValue t) => Ix -> Traversal' t Value
toLens (N n) = nth n
toLens (K k) = key k
setj :: Ix -> Value -> Value -> Value
setj = set . toLens
remove :: Ix -> Value -> Value
remove (K k) (Object h) = Object $ delete k h
remove (N i) (Array v) = Array $ ifilter (const . (/= i)) v
remove _ v = v
findAtPath :: [Ix] -> Value -> Maybe Value
findAtPath (p:ps) j = findAtPath ps =<< j ^? toLens p
findAtPath [] o = Just o

fAtPathA :: Applicative f => (Value -> f Value) -> [Ix] -> Value -> f Value
fAtPathA f (p:ps) = toLens p %%~ fAtPathA f ps
fAtPathA f [] = f

setAtPathA :: Applicative f => f Value -> [Ix] -> Value -> f Value
setAtPathA v = fAtPathA $ const v

addAtPathA :: Applicative f => [Ix] -> f Value -> Value -> f Value
addAtPathA [p] v j = set (toLens p) <$> v <*> pure j
addAtPathA (p:ps) v j = toLens p %%~ addAtPathA ps v $ j
addAtPathA [] v _ = v

fAtPath :: (Value -> Value) -> [Ix] -> Value -> Value
fAtPath f ps = runIdentity . fAtPathA (Identity . f) ps

setAtPath :: Value -> [Ix] -> Value -> Value
setAtPath v = fAtPath $ const v

addAtPath :: [Ix] -> Value -> Value -> Value
addAtPath ps v = runIdentity . addAtPathA ps (Identity v)

findAndDelete :: [Ix] -> Value -> Maybe (Value,Value)
findAndDelete [p] o = do el <- o ^? toLens p
                         return (el, remove p o)
findAndDelete (p:ps) o = do s <- o ^? toLens p
                            (id *** setj p o) <$> findAndDelete ps s
findAndDelete [] _ = Nothing

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
  (Just v) -> if t == v || show t == show v
              then return obj
              else Left $ "Value at path " <> fromPath p <> " is "
                   <> show v <> " not " <> show t
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
