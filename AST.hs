{-# LANGUAGE OverloadedStrings #-}
module AST ( parseDirsToMap
           )
       where

import Control.Applicative ((<$>), (<*>))

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Encoding    as T
import qualified Data.Vector                as V
import qualified Data.Map                   as M

import qualified Data.Text.Internal         as Strict (Text(..))
import qualified Data.HashMap.Strict        as Strict

import Data.Aeson
o
data AST = Field T.Text T.Text | Dir T.Text [AST] deriving (Show, Read, Eq)

parseDirsToMap :: T.Text -> Maybe (M.Map T.Text T.Text)
parseDirsToMap str = astToMap <$> objToAST <$> decode (T.encodeUtf8 str)

objToAST :: Value -> AST
objToAST (Object v) = Dir "" $ map valToAST $ Strict.toList v
objToAST _ = undefined

valToAST :: (Strict.Text, Value) -> AST
valToAST (id,(Object v)) = Dir (T.fromStrict id) $
                           map valToAST $
                           Strict.toList v
valToAST (id,obj) = Field (T.fromStrict id) $ objToText obj

objToText :: Value -> T.Text
objToText (String t) = T.fromStrict t
objToText (Array v) = T.pack $ show v
objToText (Number n) = T.pack $ show n
objToText (Bool b) = T.pack $ show b
objToText Null = ""
objToText _ = error "Invalid obj"

astToLMap :: AST -> [(T.Text,T.Text)]
astToLMap (Field id val) = [(id,val)]
astToLMap (Dir id vals) = map (\(i,v) -> (id `T.append` "/" `T.append` i,v)) $
                          vals >>= astToLMap

astToMap :: AST -> M.Map T.Text T.Text
astToMap = M.fromList . astToLMap

jsonTest :: BS.ByteString
jsonTest = BS.pack "{ \"a\" : \"abc\", \"foo\" : { \"bar\" : \"baz\", \"qux\" : \"quux\"}}"
