module MiniSQL where

import Control.Exception
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

type Table = String
type Column = String
data Value = TEXT String 
           | DOUBLE Double
           | BOOL Bool
           | NILV deriving (Show)
data QueryDSL = CREATE Table [Column]
              | DROP Table
              | INSERT Table {-[Column]-} [Value]
              | DELETE Table ConditionDSL
              | SELECT Table {-[Column]-} ConditionDSL
--              | UPDATE Table {-[Column]-} [Value] ConditionDSL
              | NILQ deriving (Show)
data ConditionDSL = WHERE Column OperatorDSL Value
--                  | AND ConditionDSL ConditionDSL
--                  | OR ConditionDSL ConditionDSL
--                  | NOT ConditionDSL
                  | NILC deriving (Show)
data OperatorDSL = EQO | LTO | GTO deriving (Show)

-- ErrorSQL
data ErrorSQL = CreateError
              | DropError
              | InsertError
              | DeleteError
              | SelectError
              | ConditionError
              | ReadError
              | WriteError
              | OtherError deriving (Show)

type TableRep = [(Int, [(Int, Maybe Value)])]

-- MonadSQL
type MonadSQL s v = StateT s (ExceptT ErrorSQL IO) v

-- runMonadSQL
--   runStateT  :: MonadSQL s v -> s -> ExceptT ErrorSQL (IO (v,s))
--   runExceptT :: ExceptT ErrorSQL (IO (v,s)) -> IO (Either ErrorSQL (v,s))
runMonadSQL :: MonadSQL s v -> s -> IO (Either ErrorSQL (v,s))
runMonadSQL m = runExceptT . runStateT m

-- runMonadSQL_
--   evalStateT  :: MonadSQL s v -> s -> ExceptT ErrorSQL (IO v)
--   runExceptT :: ExceptT ErrorSQL (IO (v,s)) -> IO (Either ErrorSQL v)
runMonadSQL_ :: MonadSQL s v -> s -> IO (Either ErrorSQL v)
runMonadSQL_ m = runExceptT . evalStateT m

-- MiniSQL class
--   'a' is used to represent the database (and aditional info as desired by the implementation)
class MiniSQL a where
    evalQuery :: QueryDSL -> MonadSQL a (Maybe TableRep)
    createFile :: FilePath -> MonadSQL a ()
    tryRead :: FilePath -> MonadSQL a ()
    tryWrite :: MonadSQL a ()

