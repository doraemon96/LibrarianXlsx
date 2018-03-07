module LibrarianXlsx where

-- Console Imports
import System.Console.ANSI
import System.Console.Readline
-- Parser Imports
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
-- Xlsx Imports
import qualified XlsxQueryDSL as XQ

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
          readevalprint

readevalprint :: IO ()
readevalprint = do r <- readline $ p_ps "" --TODO: put current file as arg to p_ps (get from state)
                   case r of
                       Nothing           -> putStrLn p_help
                       Just p_help       -> runHelp
                       Just p_help_query -> runHelpQuery
                       Just l            -> case parseCmdLibXslx l of
                                                Left err  -> putStrLn "Unparseable"
                                                Right cmd -> runCmdLibXslx cmd
                                                             readevalprint

runHelp :: IO ()
runHelp = do putStrLn ""
             putStrLn "Commands: (-cmd <arg1> .. <argn> : description)"
             putStrLn "    help [query]: Print this command list or a specific help topic"
             putStrLn "    lodf <filename>: Load an xlsx database file"
             putStrLn "    crea <filename>: Create a new empty xlsx database file"
             putStrLn "    runq <xlsxquery>: Run a XlsxQueryDSL query on the current database file"
             putStrLn "    quit : Exit this program"
             putStrLn "    "

runHelpQuery :: IO()
runHelpQuery = do putStrLn ""
                  putStrLn "The current possible querys are structured as follows:"
                  putStrLn "    CREATE table_name (column_name)+"
                  putStrLn "    DROP table_name"
                  putStrLn "    INSERT table_name (column_name)+ (data_elem)+"
                  putStrLn "    DELETE table_name (NOT? condition ((AND|OR) condition)*)"
                  putStrLn "    SELECT table_name (column_name)+ (NOT? condition ((AND|OR) condition)*)"
                  putStrLn "    UPDATE table_name (column_name)+ (data_elem)+ (NOT? condition ((AND|OR) condition)*)"
                  putStrLn "    "


---- parseLang
----   Parser for the console (and to create queries)
----parseLang :: 
--parseLang = emptyDef { Token.reservedNames = ["CREATE"
--                                             ,"DROP"
--                                             ,"INSERT"
--                                             ,"DELETE"
--                                             ,"SELECT"
--                                             ,"UPDATE"
--                                             ,"WHERE"
--                                             ]
--                     , Token.reservedOpNames = ["AND"
--                                               ,"OR"
--                                               ,"NOT"]}
--
----lexer :: 
--lexer = Token.makeTokenParser parseLang
--
--parser :: Parser XQ.QueryDSL
--parser = whiteSpace >> command
--
--command :: Parser XQ.QueryDSL
--command = parens command
--          <|> commandSeq
--
--command' :: Parser XQ.QueryDSL
--command' = createCmd
--           try <|> dropCmd
--           <|> insertCmd
--           <|> deleteCmd
--           <|> selectCmd
--           <|> updatecmd
--
--createCmd :: Parser XQ.QueryDSL
--createCmd = do reserved "CREATE"
--               t  <- tablename
--               cs <- columns
--               return $ CREATE t cs [] --TODO: nullable types?
--
--dropCmd :: Parser XQ.QueryDSL
--dropCmd = do reserved "DROP"
--             t  <- tablename
--             return $ DROP t
--
--insertCmd :: Parser XQ.QueryDSL
--insertCmd = do reserved "INSERT"
--               t  <- tablename
--               cs <- columns
--               vs <- values
--               return $ INSERT t cs vs
--
--deleteCmd :: Parser XQ.QueryDSL
--deleteCmd = do reserved "DELETE"
--               t  <- tablename
--               c  <- condition
--               return $ DELETE t c
--
--selectCmd :: Parser XQ.QueryDSL
--selectCmd = do reserved "SELECT"
--               t  <- tablename
--               cs <- columns
--               c  <- condition
--               return $ SELECT t cs c
--
--updateCmd :: Parser XQ.QueryDSL
--updateCmd = do reserved "UPDATE"
--               t  <- tablename
--               cs <- columns
--               vs <- values
--               c  <- condition
--               return $ UPDATE t cs vs c
--
--tablename :: Parser XQ.Table
--tablename = undefined
--
--columns :: Parser [XQ.ColName]
--columns = undefined
--
--condition :: Parser XQ.ConditionDSL
--condition = undefined
--
--values :: Parser [XQ.Value]
--values = undefined
