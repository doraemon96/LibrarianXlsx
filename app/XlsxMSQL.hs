module XlsxMSQL where

import qualified MiniSQL as MS

import Codec.Xlsx
import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (pack, unpack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Map.Lazy (lookupMax)

import Control.Exception
import Data.Typeable
import Control.Monad.Except (throwError)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)


data XlsxState = XlsxState {path :: String
                           ,xlsx :: Xlsx} deriving (Show)

defxlsxs :: XlsxState
defxlsxs = XlsxState {path = "default.xlsx", xlsx = def}

instance MS.MiniSQL XlsxState where
    evalQuery (MS.CREATE t cs) = do addSheet t
                                    writeRow 1 t (map MS.TEXT cs)
                                    return Nothing
    evalQuery (MS.DROP t)      = do deleteSheet t
                                    return Nothing
    evalQuery (MS.INSERT t vs) = do row <- getLastRow t
                                    writeRow (row+1) t vs
                                    return Nothing
    evalQuery (MS.DELETE t co) = do ec <- funConditionDSL t co
                                    filterDelSheet t ec
                                    return Nothing
    evalQuery (MS.SELECT t co) = do ec <- funConditionDSL t co
                                    filterRetSheet t ec
    evalQuery (MS.NILQ)        = return Nothing
    createFile fp = do ct <- liftIO getPOSIXTime
                       f  <- liftIO $ (try (BL.writeFile fp (fromXlsx ct def)) :: IO (Either IOException ()))
                       case f of
                           Left _ -> lift $ throwError MS.CreateError
                           _      -> do put $ XlsxState {path = fp, xlsx = def}
    tryRead fp    = do f <- liftIO $ (try (BS.readFile fp) :: IO (Either IOException BS.ByteString))
                       case f of
                           Left _   -> lift $ throwError MS.ReadError
                           Right f' -> do st <- get
                                          put $ st {path = fp , xlsx = toXlsx (BL.fromStrict f')}
    tryWrite      = do st <- get
                       ct <- liftIO getPOSIXTime
                       f <- liftIO $ (try (BL.writeFile (path st) (fromXlsx ct (xlsx st))) :: IO (Either IOException ()))
                       case f of
                           Left _ -> lift $ throwError MS.WriteError
                           _      -> return ()


-- funConditionDSL
--   Creates a useful condition checker from a ConditionDSL expression
funConditionDSL :: MS.Table -> MS.ConditionDSL -> MS.MonadSQL XlsxState (Int -> Maybe CellValue -> Bool)
funConditionDSL t (MS.WHERE col op val) = do ncol <- getColumn t col
                                             let wval = Just $ toXlsxType val
                                             case op of
                                                 MS.EQO -> return (\i mc -> (i == ncol) && (mc == wval))
                                                 MS.LTO -> return (\i mc -> (i == ncol) && (mc < wval))
                                                 MS.GTO -> return (\i mc -> (i == ncol) && (mc > wval))
funConditionDSL _ (MS.NILC)             = return (\i mc -> True)



{- ### Xlsx Wrappers ### -}

-- toXlsxType
--   converts a value into a xlsx cellvalue
toXlsxType :: MS.Value -> CellValue
toXlsxType (MS.TEXT s)   = CellText (pack s)
toXlsxType (MS.DOUBLE d) = CellDouble d
toXlsxType (MS.BOOL b)   = CellBool b

-- fromXlsxType
--   converts a maybe xlsx cellvalue into a maybe value
fromXlsxType :: Maybe CellValue -> Maybe MS.Value
fromXlsxType (Just (CellText t))   = Just $ MS.TEXT (unpack t)
fromXlsxType (Just (CellDouble d)) = Just $ MS.DOUBLE d
fromXlsxType (Just (CellBool b))   = Just $ MS.BOOL b
fromXlsxType _                     = Nothing

-- addSheet
--   creates a new sheet in the xlsx file
addSheet :: MS.Table -> MS.MonadSQL XlsxState ()
addSheet t = do st <- get
                put $ st {xlsx = atSheet (pack t) .~ Just def $ (xlsx st)}

-- deleteSheet
--   removes an entire sheet from the xlsx file
deleteSheet :: MS.Table -> MS.MonadSQL XlsxState ()
deleteSheet t = do st <- get
                   let filf = \(name,worksheet) -> name /= (pack t)
                   put $ st {xlsx = xlSheets .~ (filter filf ((xlsx st) ^. xlSheets)) $ (xlsx st)}

-- readCell
--   Returns the contents of a cell or Nothing if empty/unreadable
readCell :: MS.Table -> (Int,Int) -> MS.MonadSQL XlsxState (Maybe CellValue)
readCell t c = do st <- get
                  return $ (xlsx st) ^? ixSheet (pack t) . ixCell c . cellValue . _Just

-- writeCell
--   writes a cell in a sheet with a particular value
writeCell :: MS.Table -> (Int,Int) -> CellValue -> MS.MonadSQL XlsxState ()
writeCell t c v = do st <- get
                     let cellv = def {_cellValue = Just v}
                     put $ st {xlsx = atSheet (pack t) . _Just . atCell c .~ Just cellv $ (xlsx st)}

-- writeRow
--   writes a sheet's row from the beginning with particular values
writeRow :: Int -> MS.Table -> [MS.Value] -> MS.MonadSQL XlsxState ()
writeRow i t vs = let zvs = zip vs [1..]
                  in mapM_ (\(v,j) -> writeCell t (i,j) (toXlsxType v)) zvs

-- getLastRow
--   returns number of the last written row
getLastRow :: MS.Table -> MS.MonadSQL XlsxState Int
getLastRow t = do st <- get
                  let map = (xlsx st) ^. (ixSheet (pack t)) . wsCells
                  return $ maybe 1 (fst . fst) (lookupMax map)

-- getColumn
--   Returns the column number by its name
getColumn :: MS.Table -> MS.Column -> MS.MonadSQL XlsxState Int
getColumn = getColumn' 1 

getColumn' :: Int -> MS.Table -> MS.Column -> MS.MonadSQL XlsxState Int
getColumn' i t c = do st <- get
                      mc <- readCell t (1,i)
                      case mc of
                          Just v  -> if v == (CellText (pack c))                  
                                     then return i
                                     else getColumn' (i+1) t c
                          Nothing -> return 0

-- filterDelSheet
--   deletes all rows that don't pass a condition. Returns the new Xlsx
filterDelSheet :: MS.Table -> (Int -> (Maybe CellValue) -> Bool) -> MS.MonadSQL XlsxState ()
filterDelSheet t pred = do st <- get
                           let (_,xls) = filterSheet (xlsx st) t pred
                               xls' = fromRows (normalizeRows xls)
                           put $ st {xlsx = atSheet (pack t) . _Just . wsCells .~ xls' $ (xlsx st)}

-- filterRetSheet
--   returns all rows that pass a condition as a MS.Value list
filterRetSheet :: MS.Table -> (Int -> (Maybe CellValue) -> Bool) -> MS.MonadSQL XlsxState (Maybe MS.TableRep)
filterRetSheet t pred = do st <- get
                           let mapf = map (\(i,xs) -> (i, map (\(j,c) -> (j, fromXlsxType (c ^? cellValue . _Just))) xs))
                           return $ Just (mapf $ fst $ filterSheet (xlsx st) t pred)

-- filterSheet
--   filters over a worksheet's cells, returns (pred_true, pred_false)
filterSheet :: Xlsx -> MS.Table -> (Int -> (Maybe CellValue) -> Bool) -> ([(Int, [(Int, Cell)])], [(Int, [(Int, Cell)])])
filterSheet db sheet pred = let cells  = toRows $ db ^. (ixSheet (pack sheet)) . wsCells
                            in filterRows pred cells

filterRows :: (Int -> (Maybe CellValue) -> Bool) -> [(Int, [(Int, Cell)])] -> ([(Int, [(Int, Cell)])], [(Int, [(Int, Cell)])])
filterRows _ []        = ([],[])
filterRows pred (x:xs) = if or (map (\(i,c) -> pred i (c ^? cellValue . _Just)) cs)
                         then (x:t, f)
                         else (t, x:f)
                            where (_,cs) = x
                                  (t,f) = filterRows pred xs

-- normalizeRows
--   rearranges indexes in a list
normalizeRows :: [(Int, [(Int, Cell)])] -> [(Int, [(Int, Cell)])]
normalizeRows = normalizeRows' 1

normalizeRows' :: Int -> [(Int, [(Int, Cell)])] -> [(Int, [(Int, Cell)])]
normalizeRows' _ []     = []
normalizeRows' i (x:xs) = (i,cs) : (normalizeRows' (i+1) xs)
                            where (_,cs) = x



--getLastRow :: Xlsx -> XlsxSheet -> Int
--getLastRow db sheet = let map = db ^. (ixSheet (pack sheet)) . wsCells
--                      in maybe 0 (fst . fst) (C.lookupMax map)

