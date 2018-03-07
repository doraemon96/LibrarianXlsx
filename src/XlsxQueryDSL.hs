module XlsxQueryDSL where
-- Xlsx Imports
import qualified XlsxWrappers as XW
import XlsxTypes
import Codec.Xlsx.Types.Common
-- Monad Imports
import Control.Monad.Trans.Class
import Control.Monad.State.Class

data Type = TTEXT | TDOUBLE | TBOOL
data Value = TEXT String | DOUBLE Double | BOOL Bool
data Nullable t = NULL t | NONULL t
type ColName = String
type Table = String

data QueryDSL = CREATE Table [ColName] [Nullable Type]
               | DROP Table
               | INSERT Table [ColName] [Value]
               | DELETE Table ConditionDSL
               | SELECT Table [ColName] ConditionDSL
               | UPDATE Table [ColName] [Value] ConditionDSL
               | NILQ
data ConditionDSL = WHERE ColName OperatorDSL Value
                  | AND ConditionDSL ConditionDSL
                  | OR ConditionDSL ConditionDSL
                  | NILC
data OperatorDSL = EQO | LTO | GTO


-- evalQueryDsl
--   Implements the QueryDSL expression evaluator
--   TODO: In CREATE check for a NONULL column (use ts)
evalQueryDsl :: QueryDSL -> TryQuery ([(Int, [(Int, Maybe CellValue)])])
evalQueryDsl (CREATE t cs ts)      = do XW.addSheet t
                                        writeRow 1 t (map TEXT cs)
                                        return []
evalQueryDsl (DROP t)              = do XW.deleteSheet t
                                        return []
evalQueryDsl (INSERT t cs vs)      = do {-check last empty row (maybe with filterDelSheet?)-}
                                        writeRow 2 t vs {-TODO: use last empty row instead of 2-}
                                        {-write that row-}
                                        return []
evalQueryDsl (DELETE t cond)       = do XW.filterDelSheet t (evalConditionDSL cond)
                                        return []
evalQueryDsl (SELECT t cs cond)    = XW.filterRetSheet t (evalConditionDSL cond)
                                    {-TODO: take only cs columns-}
evalQueryDsl (UPDATE t cs ds cond) = undefined
                                    {-iter updating for condition-}
evalQueryDsl (NILQ)                = return []
                                    {-tryWrite here?-}

-- evalConditionDSL
--   Creates a useful condition checker from a ConditionDSL expression
evalConditionDSL :: ConditionDSL -> (Int -> Maybe CellValue -> Bool)
evalConditionDSL (WHERE col op val)         = let ncol = 2{-TODO: search for col :: ColName and get its number-}
                                                  wval = Just $ toXlsxType val
                                              in case op of
                                                    EQO -> (\i mc -> (not (i == ncol)) || (mc == wval))
                                                    LTO -> (\i mc -> (not (i == ncol)) || (mc < wval))
                                                    GTO -> (\i mc -> (not (i == ncol)) || (mc > wval))
evalConditionDSL (NILC)                     = (\i mc -> True)


-- writeRow
--   Writes a row of a sheet with data
writeRow :: Int -> Table -> [Value] -> TryQuery ()
writeRow i t cs = let zcs = zip cs [1..]
                  in mapM_ (\(c,j) -> XW.writeCell t (i,j) (toXlsxType c)) zcs

-- toXlsxType
toXlsxType :: Value -> CellValue
toXlsxType (TEXT t)   = XW.toXlsxText t
toXlsxType (DOUBLE d) = XW.toXlsxDouble d
toXlsxType (BOOL b)   = XW.toXlsxBool b

---- writeCols
----   Writes the first row of a sheet with column names
----   TODO: Generalize to add rows. Maybe include a row num parameter?
--writeCols :: Table -> [Column] -> IO ()
--writeCols t cs = let zcs = zip cs [1..]
--                 mapM (\(c,i) -> writeCell {-file-} t {-cell-} c) cs

