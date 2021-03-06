{-# LANGUAGE RankNTypes, TupleSections, NoMonomorphismRestriction #-}
module LensPatch (
  -- * Applying JSON Patches <http://jsonpatch.com>
  patch,
  -- * Helpers
  -- ** Misc
  toLens,
  setj,
  remove,
  add,
  -- ** Unsafe traversals
  findAtPath,
  fAtPath,
  addAtPath,
  setAtPath,
  -- ** Unsafe Applicative Traversals
  fAtPathA,
  addAtPathA,
  setAtPathA,
  -- ** Safe Traversals
  safeFAtPath,
  safeAddAtPath,
  safeSetAtPath
  ) where

import           Prelude hiding (foldr, foldl, foldr1, foldl1)
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Foldable
import           Data.HashMap.Strict (insert, delete)
import           Data.Monoid
import qualified Data.Vector as V

import           ParsePatch

-- |Converts an Ix value to a JSON lens
--
-- > toLens (N n) = nth n
-- > toLens (K k) = key k
toLens :: (AsValue t) => Ix -> Traversal' t Value
toLens (N n) = nth n
toLens (K k) = key k

navPath :: (Foldable t, Applicative f) => t Ix -> (Value -> f Value) -> Value -> f Value
navPath = foldl' (\acc p -> acc . toLens p) id

-- |Converts an Ix value to a setter lens
--
-- > setj = set ∘ toLens
setj :: Ix -> Value -> Value -> Value
setj = set . toLens

-- |Removes the value at an Ix from a value, deleting nothing if the value isn't indexable
--
-- > remove (K k) (Object h) = Object $ delete k h
-- > remove (N i) (Array v) = Array $ ifilter (const ∘ (≢ i)) v
-- > remove _ v = v
remove :: Ix -> Value -> Value
remove (K k) (Object h) = Object $ delete k h
remove (N i) (Array v) = Array $ V.ifilter (const . (/= i)) v
remove _ v = v

-- |Adds a Value to an Ix within another Value, replacing whatever was already there
add :: Value -> Ix -> Value -> Value
add v (K k) (Object o) = Object $ insert k v o
add v (N i) (Array a) = if V.length a >= i
                        then Array $ V.concat [first, V.fromList [v], rest]
                        else error "Index out of bounds error"
  where (first, rest) = V.splitAt i a

-- |Traverses through a hierarchy of JSON values and returns what it finds
findAtPath :: [Ix] -> Value -> Maybe Value
findAtPath ps j = j ^? navPath ps

-- |Applies an applicative operation and stores the new value in the old context
fAtPathA :: Applicative f => (Value -> f Value) -> [Ix] -> Value -> f Value
fAtPathA f ps = navPath ps %%~ f

-- |Inserts an applicative value and returns the new value in the old context
setAtPathA :: Applicative f => f Value -> [Ix] -> Value -> f Value
setAtPathA v = fAtPathA $ const v

-- |Adds an applicative value at a given path, adding a key if there wasn't one already
addAtPathA :: Applicative f => [Ix] -> f Value -> Value -> f Value
addAtPathA [p] v j = add <$> v <*> pure p <*> pure j
addAtPathA (p:ps) v j = toLens p %%~ addAtPathA ps v $ j
addAtPathA [] v _ = v

-- |Applies a normal function to the value at a given path
fAtPath :: (Value -> Value) -> [Ix] -> Value -> Value
fAtPath f ps = runIdentity . fAtPathA (Identity . f) ps

-- |Inserts a normal value to a given path
setAtPath :: Value -> [Ix] -> Value -> Value
setAtPath v = fAtPath $ const v

-- |Adds a normal value at the given path
addAtPath :: [Ix] -> Value -> Value -> Value
addAtPath ps v = runIdentity . addAtPathA ps (Identity v)

-- |Only returns if it can make the full traversal
--safeFAtPath :: (Foldable t, Applicative f) => (Value -> Value) -> t Ix -> Value -> f Value
safeFAtPath f = foldr (traverseOf . toLens) (pure . f)

-- |Only returns if it can make the full traversal
safeAddAtPath :: Applicative f => [Ix] -> Value -> Value -> f Value
safeAddAtPath [p] v = pure . add v p
safeAddAtPath (p:ps) v = toLens p %%~ safeAddAtPath ps v
safeAddAtPath [] v = pure . const v

-- |Only returns if it can make the full traversal
safeSetAtPath :: Applicative f => Value -> [Ix] -> Value -> f Value
safeSetAtPath v = safeFAtPath $ const v

infixl 0 <$$>
(<$$>) = (<$>)

findAndDelete :: [Ix] -> Value -> Maybe (Value,Value)
findAndDelete [p] o = (,remove p o) <$> o ^? toLens p
findAndDelete (p:ps) o = _2 %~ setj p o <$$> o ^? toLens p >>= findAndDelete ps
findAndDelete [] _ = Nothing

deleteAtPath :: [Ix] -> Value -> Maybe Value
deleteAtPath [p] = Just . remove p
deleteAtPath (p:ps) = toLens p %%~ deleteAtPath ps

-- |Applies a single JSON patch
patch :: Operation -> Value -> Either String Value
patch (Add p v) obj = case safeAddAtPath p v obj of
  (Just a) -> Right a
  Nothing -> Left $ "Nothing to remove at " <> fromPath p
patch (Rem p) obj = case snd <$> findAndDelete p obj of
  (Just a) -> Right a
  Nothing -> Left $ "Nothing to remove at " <> fromPath p
patch (Cop p1 p2) obj = case findAtPath p1 obj of
  (Just old) -> patch (Add p2 old) obj
  Nothing  -> Left $ "Couldn't find value at " <> fromPath p1
patch (Mov p1 p2) obj = case findAndDelete p1 obj of
  (Just (old,new)) -> patch (Add p2 old) new
  Nothing -> Left $  "Can't find value at path " <> fromPath p1
patch (Rep p v) obj = case safeSetAtPath v p obj of
  (Just a) -> Right a
  Nothing -> Left $ "Can't find value at path " <> fromPath p
patch (Tes p t) obj = case findAtPath p obj of
  (Just v) -> if t == v
              then return obj
              else Left $ "Value at path " <> fromPath p <> " is "
                   <> show v <> " not " <> show t
  Nothing -> Left $ "Couldn't find value at path " <> fromPath p

fromPath :: (Functor f, Foldable f, Show b) => f b -> String
fromPath = foldr1 (<>)  . fmap (cons '/' . show)
