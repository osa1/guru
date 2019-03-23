module Guru (run) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.Process.Typed

import Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Gdb.Parser as Gdb
import qualified Widgets.Gdb as GdbW

run :: [String] -> IO ()
run gdb_args = do
    app <- new Gtk.Application
      [ #applicationId := "guru.guru"
      , #flags := [ Gio.ApplicationFlagsFlagsNone ]
      ]
    void (on app #activate (activate app gdb_args))
    void (Gio.applicationRun app Nothing)

activate :: Gtk.Application -> [String] -> IO ()
activate app gdb_args = do
    w <- new Gtk.ApplicationWindow
      [ #application := app
      , #title := "Guru"
      , #defaultHeight := 200
      , #defaultWidth := 200
      ]

    -- Create the GDB widget
    gdb_widget <- GdbW.build
    GdbW.getGtkWidget gdb_widget >>= #add w

    #showAll w

    -- Spawn a GDB process
    let p = setStdin createPipe $
            setStdout createPipe $
            setStderr createPipe $
            proc "gdb" (["-n", "-i=mi", "--args"] <> gdb_args)

    GdbW.enterConnectedState gdb_widget

    _ <- forkIO $
           (withProcess_ p (\p_ -> runGdbProcess p_ gdb_widget)
              `finally` addIdle (GdbW.enterDisconnectedState gdb_widget))
              `catch` \(e :: ExitCodeException) ->
                          addIdle (GdbW.addError gdb_widget (T.pack (show e)))

    return ()

addIdle :: IO () -> IO ()
addIdle f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT_IDLE (f >> return False))

runGdbProcess :: Process Handle Handle Handle -> GdbW.GdbWidget -> IO ()
runGdbProcess p w = do
    _ <- forkIO (gdbStderrListener (getStderr p) w `finally` putStrLn "stderr listener returned")
    gdbStdoutListener (getStdout p) w `finally` putStrLn "stdout listener returned"

gdbStdoutListener :: Handle -> GdbW.GdbWidget -> IO ()
gdbStdoutListener h w = loop (parse mempty)
  where
    parse = A.parse Gdb.parse

    loop (A.Fail _unconsumed _ctx err) =
      addIdle (GdbW.addError w (T.pack err))

    loop (A.Partial cont) =
      BS.hGetSome h 10000 >>= loop . cont

    loop (A.Done unconsumed ret) = do
      addIdle (forM_ ret (GdbW.addParsedMsg w . T.pack . show))
      loop (parse unconsumed)

gdbStderrListener :: Handle -> GdbW.GdbWidget -> IO ()
gdbStderrListener h w = loop
  where
    -- TODO: When does this terminate?
    loop = do
      err <- T.hGetLine h
      addIdle (GdbW.addStderrMsg w err)
      loop
