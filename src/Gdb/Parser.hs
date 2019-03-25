module Gdb.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char, digit, notChar, anyChar)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Functor
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word

import qualified Gdb.Syntax as Gdb

{-
[NOTES]
~~~~~~~

[29/01/2019] gdb-mi documentation is wrong about the syntax: a single message
may contain out-of-band records, then a result, then more out-of-band records.
It's also wrong about the message terminator: it says "(gdb)\n" terminates the
message, but in reality there may be whitespace between "(gdb)" and "\n".

[30/01/2019] Here's another bug with gdb mi: when a breakpoint location causes
adding multiple breakpoints the notification is printed like this:

     =breakpoint-created,bkpt={...},{...},{...}

But this is not valid async record syntax. The results should be in `x=y` format
so the breakpoints after the first one (those without `bkpt=`) are not valid.


TODO: Replace M.fromLists with a fromList that checks for duplicate keys
-}

-- | Parse a single gdb message.
parse :: Parser [Gdb.Out]
parse = do
    msgs <- many parseOut
    _ <- string "(gdb) \n"
    return msgs

parseOut :: Parser Gdb.Out
parseOut = Gdb.Out <$> optional parseToken <*> parseResultOrOOB

parseToken :: Parser Int
parseToken = read <$> many1 digit

parseResultOrOOB :: Parser Gdb.ResultOrOOB
parseResultOrOOB = choice
    [ char '^' >>
        Gdb.Result <$> parseClass <*> parseVals
    , char '*' >>
        Gdb.OOB . Gdb.ExecAsyncRecord <$> parseAsyncRecord
    , char '+' >>
        Gdb.OOB . Gdb.StatusAsyncRecord <$> parseAsyncRecord
    , char '=' >>
        Gdb.OOB . Gdb.NotifyAsyncRecord <$> parseAsyncRecord
    , char '~' >>
        Gdb.OOB . Gdb.ConsoleStreamRecord <$> parseConst
    , char '@' >>
        Gdb.OOB . Gdb.TargetStreamRecord <$> parseConst
    , char '&' >>
        Gdb.OOB . Gdb.LogStreamRecord <$> parseConst
    ] <* char '\n'

parseClass :: Parser Gdb.ResultClass
parseClass = choice
    [ string "done" $> Gdb.Done
    , string "running" $> Gdb.Running
    , string "connected" $> Gdb.Connected
    , string "error" $> Gdb.Error
    , string "exit" $> Gdb.Exit
    ]

-- TODO: This silently carries on if we see a Var multiple times, which should
-- not happen, but who knows, gdb is weird, and mi is buggy.
parseVals :: Parser (M.Map Gdb.Var Gdb.Val)
parseVals = M.fromList <$> many (char ',' >> parseResult)

parseResult :: Parser (Gdb.Var, Gdb.Val)
parseResult = do
    var <- parseVar
    _ <- char '='
    val <- parseVal
    return (var, val)

parseVar :: Parser Gdb.Var
parseVar = decodeUtf8 . BS.pack <$> many1 (satisfy (\c -> c /= c2b ',' && c /= c2b '=' && not (isSpace (b2c c))))

parseVal :: Parser Gdb.Val
parseVal = choice
    [ Gdb.Const <$> parseConst
    , Gdb.Tuple <$> parseTuple
    , Gdb.ValList <$> try parseValList
    , Gdb.ResList <$> parseResList
    ]

parseConst :: Parser T.Text
parseConst = do
    _ <- char '"'
    bs <- many constByte
    _ <- char '"'
    return (decodeUtf8 (BS.pack bs))

constByte :: Parser Word8
constByte = do
    b <- notChar '"'
    if b == '\\' then
      anyChar >>= \case
        '\\' -> return (c2b '\\')
        't' -> return (c2b '\t')
        'n' -> return (c2b '\n')
        '"' -> return (c2b '"')
        c -> fail ("Unknown escape character: " ++ show c)
    else
      return (c2b b)

parseTuple :: Parser (M.Map Gdb.Var Gdb.Val)
parseTuple = do
    _ <- char '{'
    results <- sepBy parseResult (char ',')
    _ <- char '}'
    return (M.fromList results)

parseValList :: Parser [Gdb.Val]
parseValList = do
    _ <- char '['
    vals <- sepBy parseVal (char ',')
    _ <- char ']'
    return vals

parseResList :: Parser [(Gdb.Var, Gdb.Val)]
parseResList = do
    _ <- char '['
    res <- sepBy parseResult (char ',')
    _ <- char ']'
    return res

parseAsyncRecord :: Parser Gdb.AsyncRecord
parseAsyncRecord = do
    cls <- many1 (satisfy (\c -> c /= c2b '\n' && c /= c2b ','))
    results <- many (char ',' >> parseResult)
    return (Gdb.AsyncRecord (decodeUtf8 (BS.pack cls)) (M.fromList results))

c2b :: Char -> Word8
c2b = fromIntegral . fromEnum

b2c :: Word8 -> Char
b2c = toEnum . fromIntegral
