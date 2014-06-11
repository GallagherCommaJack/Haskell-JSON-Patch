{-# LANGUAGE OverloadedStrings #-}
module MapPatch where
import Data.Text hiding (copy, replace)
import qualified Data.Map as M
type Path = Text

type Paths = M.Map Path Text

data Operation = Add Path Text
               | Remove Path
               | Copy Path Path
               | Move Path Path
               | Replace Path Text
               | Test Path Text

add :: Path -> Text -> Paths -> Paths
add path val paths = case M.lookup path paths of
  Nothing -> M.insert path val paths
  Just pval -> error $ unpack $ Data.Text.unlines ["JSON Patch Error: Tried to insert:",
                                                  val,
                                                  "At path:",
                                                  path,
                                                  "But path was occupied with:",
                                                  pval]

remove :: Path -> Paths -> Paths
remove = M.delete

copy :: Path -> Path -> Paths -> Paths
copy from path paths = case M.lookup from paths of
  Just val -> add path val paths
  Nothing -> error $ unpack $ Data.Text.unlines
   ["JSON Patch Error: Couldn't find path:",
    from,
    "to move to:",
    path]

move :: Path -> Path -> Paths -> Paths
move from path = remove from . copy from path

replace :: Path -> Text -> Paths -> Paths
replace = M.insert

test :: Path -> Text -> Paths -> Bool
test path val = (==val) . M.findWithDefault "" path

applyOp :: Operation -> Paths -> Paths
applyOp (Add path val) = add path val
applyOp (Remove path) = remove path
applyOp (Copy from path) = copy from path
applyOp (Move from path) = move from path
applyOp (Replace path val) = replace path val
applyOp (Test path val) = \paths -> if test path val paths
                                   then paths
                                   else error $ unpack $ Data.Text.unlines
                                        ["JSON Patch Error: Wrong value at path:",
                                         path,
                                         "Expected:",
                                         val,
                                         "Found:",
                                         M.findWithDefault "" path paths] --}
