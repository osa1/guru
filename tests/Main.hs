module Main where

import Data.Attoparsec.ByteString hiding (parseOnly)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import Data.Either
import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.HUnit

import qualified Gdb.Parser as Gdb
import qualified Gdb.Syntax as Gdb

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
    [ testGroup "Parsers" parserTests ]

parseOnly :: Parser a -> BS.ByteString -> Either String a
parseOnly p = A.parseOnly (p <* endOfInput)

parserTests :: [TestTree]
parserTests =
    [ testCase "Const" $ do
        assertEqual "Const 1"  (Right "foo") (parseOnly Gdb.parseConst "\"foo\"")
        assertEqual "Const 2"  (Right "f\"o\"o") (parseOnly Gdb.parseConst "\"f\\\"o\\\"o\"")
    , testCase "Var" $ do
        assertEqual "Var 1" (Right "foo") (parseOnly Gdb.parseVar "foo")
    , testCase "Result" $ do
        assertEqual "Result 1" (Right ("foo", Gdb.Const "bar")) (parseOnly Gdb.parseResult "foo=\"bar\"")
    , testCase "Tuple" $ do
        assertEqual "Tuple 1"
          (Right (M.fromList [("foo", Gdb.Const "bar")]))
          (parseOnly Gdb.parseTuple "{foo=\"bar\"}")
        assertEqual "Tuple 2"
          (Right (M.fromList [("foo", Gdb.Const "bar"), ("baz", Gdb.Const "qux")]))
          (parseOnly Gdb.parseTuple "{foo=\"bar\",baz=\"qux\"}")
    , testCase "ValList" $ do
        assertEqual "ValList 1"
          (Right [])
          (parseOnly Gdb.parseValList "[]")
        assertEqual "ValList 2"
          (Right [Gdb.Const "foo"])
          (parseOnly Gdb.parseValList "[\"foo\"]")
        assertEqual "ValList 2"
          (Right [Gdb.Const "foo", Gdb.Const "bar"])
          (parseOnly Gdb.parseValList "[\"foo\",\"bar\"]")
    , testCase "ResList" $ do
        assertEqual "ResList 1"
          (Right [])
          (parseOnly Gdb.parseResList "[]")
    , testCase "ResList" $ do
        assertEqual "ResList 1"
          (Right [("foo", Gdb.Const "bar")])
          (parseOnly Gdb.parseResList "[foo=\"bar\"]")
    , testCase "ResList" $ do
        assertEqual "ResList 2"
          (Right [("foo", Gdb.Const "bar"), ("baz", Gdb.Const "qux")])
          (parseOnly Gdb.parseResList "[foo=\"bar\",baz=\"qux\"]")
    , testCase "Token" $ do
        assertEqual "Token 1"
          (Right 123)
          (parseOnly Gdb.parseToken "123")

    , testCase "Val" $ do
        assertBool "Val large" $
          isRight $ parseOnly Gdb.parseVal $
              "[frame={level=\"0\",addr=\"0x00000000006eff82\",func=\"initCapabilities\",file=" <>
               "\"rts/Capability.c\",fullname=\"/home/omer/haskell/ghc-gc/rts/Capability.c\"," <>
               "line=\"398\"},frame={level=\"1\",addr=\"0x00000000006ee476\",func=\"initScheduler" <>
               "\",file=\"rts/Schedule.c\",fullname=\"/home/omer/haskell/ghc-gc/rts/Schedule.c" <>
               "\",line=\"2680\"},frame={level=\"2\",addr=\"0x00000000006e8cc0\"," <>
               "func=\"hs_init_ghc\",file=\"rts/RtsStartup.c\",fullname=" <>
               "\"/home/omer/haskell/ghc-gc/rts/RtsStartup.c\",line=\"236\"},frame={level=\"3\"" <>
               ",addr=\"0x0000000000701f08\",func=\"hs_main\",file=\"rts/RtsMain.c\"," <>
               "fullname=\"/home/omer/haskell/ghc-gc/rts/RtsMain.c\",line=\"57\"}," <>
               "frame={level=\"4\",addr=\"0x0000000000405366\",func=\"main\"}]"

    , testCase "OOB or Result" $ do
        assertEqual "OOB 1"
          (Right (Gdb.OOB (Gdb.NotifyAsyncRecord (Gdb.AsyncRecord "thread-group-added" M.empty))))
          (parseOnly Gdb.parseResultOrOOB "=thread-group-added\n")
        assertEqual "OOB 2"
          (Right (Gdb.OOB (Gdb.NotifyAsyncRecord (Gdb.AsyncRecord "thread-group-added" (M.fromList [("id", Gdb.Const "i1")])))))
          (parseOnly Gdb.parseResultOrOOB "=thread-group-added,id=\"i1\"\n")
        assertEqual "Result 1"
          (Right (Gdb.Result Gdb.Done M.empty))
          (parseOnly Gdb.parseResultOrOOB "^done\n")

    , testCase "Class" $ do
        assertEqual "Result class 1"
          (Right Gdb.Done)
          (parseOnly Gdb.parseClass "done")

    , testCase "Full output" $ do
        assertEqual "Full 1"
          (Right [Gdb.Out Nothing (Gdb.Result Gdb.Done M.empty)])
          (parseOnly Gdb.parse "^done\n(gdb) \n")
    ]
