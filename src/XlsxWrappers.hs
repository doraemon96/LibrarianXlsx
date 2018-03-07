module XlsxWrappers ( tryReadXlsx
                    , tryWriteXlsx
                    , readCell
                    , writeCell
                    , deleteCell
                    , createXlsxI
                    , createXlsxU
                    , addSheet
                    , renameSheet
                    , deleteSheet
                    , filterDelSheet
                    , filterRetSheet
                    , filterSheet
                    , toXlsxText
                    , toXlsxDouble
                    , toXlsxBool) where
import Codec.Xlsx
import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.List (or, map)
import qualified Data.Map.Lazy as C
import Control.Exception (try, IOException)

import Control.Monad.State
import Control.Monad.Trans.Maybe
import XlsxTypes

-- toXlsxText
toXlsxText :: String -> CellValue
toXlsxText s = CellText (T.pack s)

-- toXlsxDouble
toXlsxDouble :: Double -> CellValue
toXlsxDouble f = CellDouble f

-- toXlsxBool
toXlsxBool :: Bool -> CellValue
toXlsxBool b = CellBool b

-- tryOpen
--   Opens a file and converts it to Xlsx (counterintuitively "ignores" bytestring lazyness)
tryReadXlsx :: FilePath -> IO (Maybe Xlsx)
tryReadXlsx fp = do f <- try (BS.readFile fp) :: IO (Either IOException BS.ByteString)
                    case f of
                        Left _  -> return $ Nothing
                        Right f -> return $ Just $ toXlsx (L.fromStrict f)

tryWriteXlsx :: FilePath -> Xlsx -> IO ()
tryWriteXlsx fp db = do ct <- getPOSIXTime
                        L.writeFile fp $ fromXlsx ct db

-- readCell
--   Returns the contents of a cell or Nothing if empty/unreadable
readCell :: Xlsx -> XlsxSheet -> XlsxCell -> Maybe CellValue
readCell db sheet cell =  db ^? ixSheet (T.pack sheet) . ixCell cell . cellValue . _Just

-- writeCell
--   Writes content to a cell and returns the new Xlsx
writeCell :: XlsxSheet -> XlsxCell -> CellValue -> TryQuery ()
writeCell sheet cell val = do st <- get
                              let cellv = Cell {_cellStyle = Nothing, _cellValue = Just val, _cellComment = Nothing, _cellFormula = Nothing}
                              put $ st {dbxlsx = atSheet (T.pack sheet) . _Just . atCell cell .~ Just cellv $ (dbxlsx st)}

writeCell' :: Xlsx -> XlsxSheet -> XlsxCell -> CellValue -> Xlsx
writeCell' db sheet cell val = let cellv = Cell {_cellStyle = Nothing, _cellValue = Just val, _cellComment = Nothing, _cellFormula = Nothing}
                              in atSheet (T.pack sheet) . _Just . atCell cell .~ Just cellv $ db

-- deleteCell
--   Deletes content from a cell (fills it with Nothing) and returns the new Xlsx
--   TODO: why is 
deleteCell :: Xlsx -> XlsxSheet -> XlsxCell -> Xlsx
deleteCell db sheet cell = atSheet (T.pack sheet) . _Just . atCell cell .~ Nothing $ db

-- createXlsxI
--   Creates or overwrites a xlsx file, initialized
createXlsxI :: FilePath -> XlsxSheet -> Worksheet -> IO ()
createXlsxI fp sheet sheetv = do let xlsx = def & atSheet (T.pack sheet) ?~ sheetv
                                 tryWriteXlsx fp xlsx

-- createXlsxU
--   Creates or overwrites a xlsx file, uninitialized
createXlsxU :: FilePath -> IO ()
createXlsxU fp = tryWriteXlsx fp def

-- addSheet
--   Creates a new sheet in a xlsx file
addSheet :: XlsxSheet -> TryQuery ()
addSheet sheet = do st <- get
                    put $ st {dbxlsx = atSheet (T.pack sheet) .~ Just def $ (dbxlsx st)}

addSheet' :: Xlsx -> XlsxSheet -> Xlsx
addSheet' db sheet = atSheet (T.pack sheet) .~ Just def $ db
                       
-- renameSheet
--   Renames an existing sheet in a xlsx file
renameSheet :: Xlsx -> XlsxSheet -> XlsxSheet -> Xlsx
renameSheet db sheet sheet' = let mapf = \(t,w) -> if t == T.pack sheet then (T.pack sheet',w) else (t,w)
                              in xlSheets .~ (map mapf (db ^. xlSheets)) $ db

-- deleteSheet
--   Deletes an existing sheet in a xlsx file
deleteSheet :: XlsxSheet -> TryQuery ()
deleteSheet sheet = do st <- get
                       let filf = \(t,w) -> t /= T.pack sheet
                       put $ st {dbxlsx = xlSheets .~ (filter filf ((dbxlsx st) ^. xlSheets)) $ (dbxlsx st)}

deleteSheet' :: Xlsx -> XlsxSheet -> Xlsx
deleteSheet' db sheet = let filf = \(t,w) -> t /= T.pack sheet
                        in xlSheets .~ (filter filf (db ^. xlSheets)) $ db

-- filterDelSheet
--   Deletes all rows that don't pass a condition. Returns the new Xlsx
filterDelSheet :: XlsxSheet -> (Int -> (Maybe CellValue) -> Bool) -> TryQuery ()
filterDelSheet sheet pred = do st <- get
                               let (_,xls) = filterSheet (dbxlsx st) sheet pred
                                   xls' = fromRows (normalizeRows xls)
                               put $ st {dbxlsx = atSheet (T.pack sheet) . _Just . wsCells .~ xls' $ (dbxlsx st)}

-- filterRetSheet
--   Returns all rows that pass a condition as a Maybe CellValue list
filterRetSheet :: XlsxSheet -> (Int -> (Maybe CellValue) -> Bool) -> TryQuery ([(Int, [(Int, Maybe CellValue)])])
filterRetSheet sheet pred = do st <- get
                               let mapf = map (\(i,xs) -> (i, map (\(j,c) -> (j, c ^? cellValue . _Just)) xs))
                               return $ mapf $ fst $ filterSheet (dbxlsx st) sheet pred

-- filterSheet
--   Filters over a worksheet's cells, returns (pred_true, pred_false)
filterSheet :: Xlsx -> XlsxSheet -> (Int -> (Maybe CellValue) -> Bool) -> ([(Int, [(Int, Cell)])], [(Int, [(Int, Cell)])])
filterSheet db sheet pred = let cells  = toRows $ db ^. (ixSheet (T.pack sheet)) . wsCells
                            in filterRows pred cells

filterRows :: (Int -> (Maybe CellValue) -> Bool) -> [(Int, [(Int, Cell)])] -> ([(Int, [(Int, Cell)])], [(Int, [(Int, Cell)])])
filterRows _ []        = ([],[])
filterRows pred (x:xs) = if or (map (\(i,c) -> pred i (c ^? cellValue . _Just)) cs)
                         then (x:t, f)
                         else (t, x:f)
                            where (_,cs) = x
                                  (t,f) = filterRows pred xs

normalizeRows :: [(Int, [(Int, Cell)])] -> [(Int, [(Int, Cell)])]
normalizeRows = normalizeRows' 1

normalizeRows' :: Int -> [(Int, [(Int, Cell)])] -> [(Int, [(Int, Cell)])]
normalizeRows' _ []     = []
normalizeRows' i (x:xs) = (i,cs) : (normalizeRows' (i+1) xs)
                            where (_,cs) = x
