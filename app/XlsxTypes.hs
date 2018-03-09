module XlsxTypes where

import Codec.Xlsx
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Except
import Control.Monad.State

-- Primitives to work with Xlsx
type XlsxCell = (Int,Int)
type XlsxSheet = String

-- Error handling
data XlsxErr = Error | NoError deriving (Show, Eq)

-- State TODO
data XlsxState = XlsxState { dbpath :: String
                           , dbxlsx :: Xlsx} deriving (Show)

-- TryQuery Monad
--   Una monada de State encapsulada en una monada de Error
--type TryQuery a = StateT XlsxState Maybe a 
-- : Maybe (State XlsxState a)
type TryQuery a = State XlsxState a
