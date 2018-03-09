module Main where

-- Console Imports
import System.Console.ANSI
import System.Console.Readline
-- Parser Imports
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Number
-- Xlsx Imports
import Codec.Xlsx.Types
import qualified XlsxQueryDSL as XQ
import qualified XlsxWrappers as XW
import XlsxTypes
-- Other Imports
import Foreign.Marshal.Error
import Control.Monad.State


version = "0.7.3"

p_ps f = "LibXlsx<" ++ f ++ ">$ "
p_help = "help"
p_help_query = "help query"


main :: IO ()
main = do setCursorPosition 0 0
          clearScreen
          putStrLn $ "## Librarian Xlsx ## (Version " ++ version ++ ")"
          putStrLn $ "## For help type \"" ++ p_help ++ "\" ##"
          putStrLn $ ""
          readevalprint $ XlsxState "default.xlsx" def

readevalprint :: XlsxState -> IO ()
readevalprint st = do r <- readline $ p_ps (dbpath st)
                      case r of
                         Nothing -> putStrLn ""
                         Just "showme" -> do putStrLn (show (dbxlsx st))
                                             readevalprint st
                         Just s  -> case parseCmd s of
                                       Right cmd -> runCmd st cmd
                                       Left err  -> do putStrLn ""
                                                       readevalprint st
                                           
runHelp :: IO ()
runHelp = do putStrLn ""
             putStrLn "Commands: (-cmd <arg1> .. <argn> : description)"
             putStrLn "    help [query|cond]: Print this command list or a specific help topic"
             putStrLn "    lodf <filename>: Load an xlsx database file"
             putStrLn "    wrif : Apply changes to current file and write"
             putStrLn "    crea <filename>: Create a new empty xlsx database file"
             putStrLn "    runq <xlsxquery>: Run a XlsxQueryDSL query on the current database file"
             putStrLn "    quit : Exit this program"
             putStrLn "    "

runHelpQuery :: IO ()
runHelpQuery = do putStrLn ""
                  putStrLn "The current possible querys are structured as follows:"
                  putStrLn "    CREATE table_name (col1 col2 ...)"
                  putStrLn "    DROP table_name"
                  putStrLn "    INSERT table_name (data1 data2 ...)"
                  putStrLn "    DELETE table_name (cond1 AND/OR cond2 AND/OR ...)"
                  putStrLn "    SELECT table_name (cond1 AND/OR cond2 AND/OR ...)"
                  putStrLn "    "

runHelpCond :: IO ()
runHelpCond = do putStrLn ""
                 putStrLn "The current possible conditions are structured as follows"
                 putStrLn "    WHERE column </>/= data"
                 putStrLn "    "

data Command = Help String 
             | LoadFile String 
             | WriteFile
             | CreateFile String 
             | RunQuery XQ.QueryDSL 
             | Quit deriving (Show)

runCmd :: XlsxState -> Command -> IO ()
runCmd st (Help "query") = do{runHelpQuery; readevalprint st}
runCmd st (Help "cond")  = do{runHelpCond; readevalprint st}
runCmd st (LoadFile f)   = do mxlsx <- XW.tryReadXlsx f
                              maybe (readevalprint st) (\x -> readevalprint (st {dbpath = f, dbxlsx = x})) mxlsx
runCmd st (WriteFile)    = do putStrLn $ "## Writing " ++ (dbpath st) ++ " ##"
                              XW.tryWriteXlsx (dbpath st) (dbxlsx st)
                              readevalprint st
runCmd st (CreateFile f) = do{XW.createXlsxU f; readevalprint (st {dbpath = f})}
runCmd st (RunQuery q)   = let (r,st') = runState (XQ.evalQueryDsl q) st
                           in case r of
                                [] -> readevalprint st'
                                xs -> do{ putStrLn (show xs); readevalprint st'}
runCmd st (Quit)         = do{putStrLn "## Closing LibrarianXlsx ##"; return ()}
runCmd st _              = do{runHelp; readevalprint st}

-- parseCmd
--   Parser for the console (and to create queries)
parseCmd :: String -> Either ParseError Command
parseCmd = parse parserC ""

lis = makeTokenParser (emptyDef { reservedNames = ["help"
                                                  ,"lodf"
                                                  ,"wrif"
                                                  ,"crea"
                                                  ,"runq"
                                                  ,"quit"
                                                  ,"CREATE"
                                                  ,"DROP"
                                                  ,"INSERT"
                                                  ,"DELETE"
                                                  ,"SELECT"
                                                  ,"WHERE"
                                             ]
                                , reservedOpNames = ["AND"
                                                    ,"OR"
                                                    ,"NOT"
                                                    ,"="
                                                    ,">"
                                                    ,"<"]})

--lexer = makeTokenParser parseLang

parserC :: Parser Command
parserC = (whiteSpace lis) >> command

command :: Parser Command
command = helpC
          <|> lodfC
          <|> wrifC
          <|> creaC
          <|> runqC
          <|> quitC

helpC :: Parser Command
helpC = do (reserved lis) "help"
           h <- do{(identifier lis) <|> (return "")}
           return $ Help h

--TODO: Not working without dot (lodf and crea)
lodfC :: Parser Command
lodfC = do (reserved lis) "lodf"
           try (do{ fi <- identifier lis ;
                    dot lis;
                    fe <- identifier lis ;
                    return $ LoadFile (fi++"."++fe)})
           <|> (do{ f <- identifier lis ;
                    return $ LoadFile f})

wrifC :: Parser Command
wrifC = do (reserved lis) "wrif"
           return $ WriteFile

creaC :: Parser Command
creaC = do (reserved lis) "crea"
           try (do{ fi <- identifier lis ;
                    dot lis;
                    fe <- identifier lis ;
                    return $ LoadFile (fi++"."++fe)})
           <|> (do{ f <- identifier lis ;
                    return $ LoadFile f})

runqC :: Parser Command
runqC = do (reserved lis) "runq"
           q <- query
           return $ RunQuery q

quitC :: Parser Command
quitC = do (reserved lis) "quit"
           return $ Quit

query :: Parser XQ.QueryDSL
query = createQ
         <|> try dropQ
         <|> insertQ
         <|> deleteQ
         <|> selectQ

createQ :: Parser XQ.QueryDSL
createQ = do (reserved lis) "CREATE"
             t  <- tablename
             cs <- columns
             return $ XQ.CREATE t cs

dropQ :: Parser XQ.QueryDSL
dropQ = do (reserved lis) "DROP"
           t  <- tablename
           return $ XQ.DROP t

insertQ :: Parser XQ.QueryDSL
insertQ = do (reserved lis) "INSERT"
             t  <- tablename
             whiteSpace lis
             vs <- values
             return $ XQ.INSERT t vs

deleteQ :: Parser XQ.QueryDSL
deleteQ = do (reserved lis) "DELETE"
             t  <- tablename
             c  <- condition
             return $ XQ.DELETE t c

selectQ :: Parser XQ.QueryDSL
selectQ = do (reserved lis) "SELECT"
             t  <- tablename
             c  <- condition
             return $ XQ.SELECT t c

tablename :: Parser String
tablename = do t <- identifier lis
               return t

columns :: Parser [String]
columns = do string "("
             manyTill (identifier lis) (try (string ")"))

condition :: Parser XQ.ConditionDSL
condition = do (reserved lis) "WHERE"
               c  <- identifier lis
               op <- conditionop
               v  <- value
               return $ XQ.WHERE c op v
            <|> return XQ.NILC

conditionop :: Parser XQ.OperatorDSL
conditionop = do{(reservedOp lis) "="; return XQ.EQO}
            <|> do{(reservedOp lis) "<"; return XQ.LTO}
            <|> do{(reservedOp lis) ">"; return XQ.GTO}

value :: Parser XQ.Value
value = try (do string "True" ; return $ XQ.BOOL True)
        <|> try (do string "False" ; return $ XQ.BOOL False)
        <|> try (do f <- floating ;
                    return $ XQ.DOUBLE f)
        <|> try (do i <- int;
                    return $ XQ.DOUBLE (fromIntegral i))
        <|> do i <- identifier lis
               return $ XQ.TEXT i

values :: Parser [XQ.Value]
values = do string "("
            manyTill (whiteSpace lis >> value) (try (string ")"))
