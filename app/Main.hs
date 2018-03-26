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
import qualified MiniSQL as MS
import qualified XlsxMSQL as XS
-- Other Imports
import Foreign.Marshal.Error
import Control.Monad.State
import Control.Monad.Except


version = "0.8.2"

p_ps f = "LibXlsx<" ++ f ++ ">$ "
p_help = "help"
p_help_query = "help query"


main :: IO ()
main = do setCursorPosition 0 0
          clearScreen
          putStrLn $ "## Librarian Xlsx ## (Version " ++ version ++ ")"
          putStrLn $ "## For help type \"" ++ p_help ++ "\" ##"
          putStrLn $ ""
          readevalprint $ XS.defxlsxs

readevalprint :: XS.XlsxState -> IO ()
readevalprint st = do r <- readline $ p_ps (XS.path st)
                      case r of
                         Nothing -> putStrLn ""
                         Just "showme" -> do putStrLn (show (XS.xlsx st))
                                             readevalprint st
                         Just s  -> case parseCmd s of
                                       Right cmd -> runCmd st cmd
                                       Left err  -> do putStrLn "(Command not supported / Parsing error)"
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
                  putStrLn "    DELETE table_name cond"
                  putStrLn "    SELECT table_name cond"
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
             | RunQuery MS.QueryDSL 
             | Quit deriving (Show)

runCmd :: XS.XlsxState -> Command -> IO ()
runCmd st (Help "query") = runHelpQuery >> readevalprint st
runCmd st (Help "cond")  = runHelpCond >> readevalprint st
runCmd st (LoadFile f)   = do e <- MS.runMonadSQL (MS.tryRead f) st
                              case e of
                                  Left err      -> putStrLn (show err) >> readevalprint st
                                  Right (_,st') -> readevalprint st'
runCmd st (WriteFile)    = do e <- MS.runMonadSQL_ (MS.tryWrite) st
                              case e of
                                  Left err -> putStrLn (show err) >> readevalprint st
                                  Right _  -> readevalprint st
runCmd st (CreateFile f) = do e <- MS.runMonadSQL (MS.createFile f) st
                              case e of
                                  Left err -> putStrLn (show err) >> readevalprint st
                                  Right (_,st') -> readevalprint st'
runCmd st (RunQuery q)   = do e <- MS.runMonadSQL (MS.evalQuery q) st
                              case e of
                                  Left err       -> putStrLn (show err) >> readevalprint st
                                  Right (mv,st') -> maybe (readevalprint st') (\x -> putStrLn (show x) >> readevalprint st') mv
runCmd st (Quit)         = putStrLn "## Closing LibriarianXlsx ##" >> return ()
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
                    return $ CreateFile (fi++"."++fe)})
           <|> (do{ f <- identifier lis ;
                    return $ CreateFile f})

runqC :: Parser Command
runqC = do (reserved lis) "runq"
           q <- query
           return $ RunQuery q

quitC :: Parser Command
quitC = do (reserved lis) "quit"
           return $ Quit

query :: Parser MS.QueryDSL
query = createQ
         <|> try dropQ
         <|> insertQ
         <|> deleteQ
         <|> selectQ

createQ :: Parser MS.QueryDSL
createQ = do (reserved lis) "CREATE"
             t  <- tablename
             cs <- columns
             return $ MS.CREATE t cs

dropQ :: Parser MS.QueryDSL
dropQ = do (reserved lis) "DROP"
           t  <- tablename
           return $ MS.DROP t

insertQ :: Parser MS.QueryDSL
insertQ = do (reserved lis) "INSERT"
             t  <- tablename
             whiteSpace lis
             vs <- values
             return $ MS.INSERT t vs

deleteQ :: Parser MS.QueryDSL
deleteQ = do (reserved lis) "DELETE"
             t  <- tablename
             c  <- condition
             return $ MS.DELETE t c

selectQ :: Parser MS.QueryDSL
selectQ = do (reserved lis) "SELECT"
             t  <- tablename
             c  <- condition
             return $ MS.SELECT t c

tablename :: Parser String
tablename = do t <- identifier lis
               return t

columns :: Parser [String]
columns = do string "("
             manyTill (identifier lis) (try (string ")"))

condition :: Parser MS.ConditionDSL
condition = do (reserved lis) "WHERE"
               c  <- identifier lis
               op <- conditionop
               v  <- value
               return $ MS.WHERE c op v
            <|> return MS.NILC

conditionop :: Parser MS.OperatorDSL
conditionop = do{(reservedOp lis) "="; return MS.EQO}
            <|> do{(reservedOp lis) "<"; return MS.LTO}
            <|> do{(reservedOp lis) ">"; return MS.GTO}

value :: Parser MS.Value
value = try (do string "True" ; return $ MS.BOOL True)
        <|> try (do string "False" ; return $ MS.BOOL False)
        <|> try (do f <- floating ;
                    return $ MS.DOUBLE f)
        <|> try (do i <- int;
                    return $ MS.DOUBLE (fromIntegral i))
        <|> do i <- identifier lis
               return $ MS.TEXT i

values :: Parser [MS.Value]
values = do string "("
            manyTill (whiteSpace lis >> value) (try (string ")"))
