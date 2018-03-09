module XlsxQueryDSL where
-- Xlsx Imports
import qualified XlsxWrappers as XW
import XlsxTypes
import Codec.Xlsx.Types.Common
-- Monad Imports
import Control.Monad.Trans.Class
import Control.Monad.State.Class

data Value = TEXT String | DOUBLE Double | BOOL Bool deriving (Show)
type ColName = String
type Table = String

data QueryDSL = CREATE Table [ColName]
               | DROP Table
               | INSERT Table [Value]
               | DELETE Table ConditionDSL
               | SELECT Table ConditionDSL
               | NILQ deriving (Show)
data ConditionDSL = WHERE ColName OperatorDSL Value
                  | NILC deriving (Show)
data OperatorDSL = EQO | LTO | GTO deriving (Show)


-- evalQueryDsl
--   Implements the QueryDSL expression evaluator
--   TODO: In CREATE check for a NONULL column (use ts)
evalQueryDsl :: QueryDSL -> TryQuery ([(Int, [(Int, Maybe CellValue)])])
evalQueryDsl (CREATE t cs)   = do XW.addSheet t
                                  writeRow 1 t (map TEXT cs)
                                  return []
evalQueryDsl (DROP t)        = do XW.deleteSheet t
                                  return []
evalQueryDsl (INSERT t vs)   = do st <- get
                                  let row = XW.getLastRow (dbxlsx st) t
                                  writeRow (row+1) t vs
                                  return []
evalQueryDsl (DELETE t cond) = do ec <- evalConditionDSL t cond
                                  XW.filterDelSheet t ec
                                  return []
evalQueryDsl (SELECT t cond) = do ec <- evalConditionDSL t cond
                                  XW.filterRetSheet t ec
evalQueryDsl (NILQ)          = return []

-- evalConditionDSL
--   Creates a useful condition checker from a ConditionDSL expression
evalConditionDSL :: Table -> ConditionDSL -> TryQuery (Int -> Maybe CellValue -> Bool)
evalConditionDSL t (WHERE col op val) = do ncol <- getColumn t col
                                           let wval = Just $ toXlsxType val
                                           case op of
                                                EQO -> return (\i mc -> (i == ncol) && (mc == wval))
                                                LTO -> return (\i mc -> (i == ncol) && (mc < wval))
                                                GTO -> return (\i mc -> (i == ncol) && (mc > wval))
evalConditionDSL _ (NILC)               = return (\i mc -> True)


-- writeRow
--   Writes a row of a sheet with data
writeRow :: Int -> Table -> [Value] -> TryQuery ()
writeRow i t cs = let zcs = zip cs [1..]
                  in mapM_ (\(c,j) -> XW.writeCell t (i,j) (toXlsxType c)) zcs

-- getColumns
--   Returns the column number by its name
getColumn :: Table -> ColName -> TryQuery Int
getColumn = getColumn' 1

getColumn' :: Int -> Table -> ColName -> TryQuery Int
getColumn' i t c = do st <- get
                      case XW.readCell (dbxlsx st) t (1,i) of
                        Just v -> if v == (XW.toXlsxText c)
                                  then return i else getColumn' (i+1) t c
                        Nothing -> return 0

-- toXlsxType
toXlsxType :: Value -> CellValue
toXlsxType (TEXT t)   = XW.toXlsxText t
toXlsxType (DOUBLE d) = XW.toXlsxDouble d
toXlsxType (BOOL b)   = XW.toXlsxBool b

