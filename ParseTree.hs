{-# LANGUAGE OverloadedStrings #-}

module ParseTree where

import Data.Aeson (Value(..), decode)

import Data.Tree

import Data.Text.Lazy (Text(..))
import Data.Text.Lazy.Encoding (encodeUtf8)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Internal         as Strict (Text(..))
import qualified Data.HashMap.Strict        as Strict

leaf :: a -> Tree a
leaf = flip Node []

objToTree :: Value -> Tree Text
objToTree (Object v) = Node "" $ map valToTree $ Strict.toList v
objToTree _ = undefined

valToTree :: (Strict.Text, Value) -> Tree Text
valToTree (id,(Object v)) = Node (T.fromStrict id) $
                            map valToTree $
                            Strict.toList v
valToTree (id,obj) = Node (T.fromStrict id) $ [leaf $ objToText obj]

objToText :: Value -> Text
objToText (String t) = T.fromStrict t
objToText (Array v) = T.pack $ show v
objToText (Number n) = T.pack $ show n
objToText (Bool b) = T.pack $ show b
objToText Null = ""
objToText _ = error "Invalid obj"

bsToTree :: BS.ByteString -> Maybe (Tree Text)
bsToTree = fmap objToTree . decode

tToTree :: Text -> Maybe (Tree Text)
tToTree = bsToTree . encodeUtf8

strToTree :: String -> Maybe (Tree Text)
strToTree = bsToTree . BS.pack

jsonTest :: BS.ByteString
jsonTest = BS.pack "{ \"a\" : \"abc\", \"foo\" : { \"bar\" : \"baz\", \"qux\" : \"quux\"}}"
