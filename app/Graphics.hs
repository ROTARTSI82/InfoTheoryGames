{-# LANGUAGE RecursiveDo #-}


module Graphics (
                enterPhase
              , gamePhase
              , endPhase)
                where

import Graphics.UI.Gtk
    ( on,
      signalDisconnect,
      containerAdd,
      containerRemove,
      deleteEvent,
      widgetShowAll,
      builderAddFromFile,
      builderGetObject,
      builderNew,
      buttonActivated,
      entryGetText,
      entrySetText,
      initGUI,
      mainGUI,
      mainQuit,
      castToButton,
      castToEntry,
      castToGrid,
      castToWindow,
      Builder )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Control.Monad

parse :: String -> String
parse raw =
    let
        repl '_' = '.'
        repl c   = c
        second (x:_:xs) = x : second xs
        second _ = []
    in
        map repl (second raw)

enterPhase :: (Builder -> String -> IO ()) -> IO ()
enterPhase readyFunc = do
    _ <- initGUI
    builder <- builderNew
    builderAddFromFile builder "UI.glade"

    enterWindow <- builderGetObject builder castToWindow "enterWindow"
    gameWindow  <- builderGetObject builder castToWindow "gameWindow"
    enterGrid   <- builderGetObject builder castToGrid "enterGrid"
    gameGrid    <- builderGetObject builder castToGrid "gameGrid"
    display     <- builderGetObject builder castToEntry "entry"

    addLetter <- builderGetObject builder castToButton "addLetter"
    _ <- addLetter `on` buttonActivated $
        entryGetText display >>= entrySetText display . (++ "_ ")

    addSpace <- builderGetObject builder castToButton "addSpace"
    _ <- addSpace `on` buttonActivated $
        entryGetText display >>= entrySetText display . (\orig ->
            if not (null orig) && last (init orig) == '_'
            then orig ++ "  "
            else orig)

    ready <- builderGetObject builder castToButton "ready"
    _ <- ready `on` buttonActivated $ do
        containerRemove enterWindow enterGrid
        containerRemove gameWindow gameGrid
        containerAdd enterWindow gameGrid
        entryGetText display >>= readyFunc builder . parse

    _ <- enterWindow `on` deleteEvent $ do
        liftIO mainQuit
        return False

    widgetShowAll enterWindow
    mainGUI

gamePhase :: Builder -> [String] -> Int -> (String -> IO ()) -> IO ()
gamePhase builder datas expectedLength next = do
    guess        <- builderGetObject builder castToEntry "guess"
    mostLikely   <- builderGetObject builder castToEntry "mostLikely"
    sndMostLikely   <- builderGetObject builder castToEntry "sndMostLikely"
    expectedInfo <- builderGetObject builder castToEntry "expectedInfo"

    input <- builderGetObject builder castToEntry "input"
    entrySetText input ""

    proceed <- builderGetObject builder castToButton "proceed"

    rec handler <- proceed `on` buttonActivated $ do {
        let
            stringLength :: String -> Int
            stringLength = length
        in do {
            text <- entryGetText input;
            Control.Monad.when (stringLength text == expectedLength) $ do
                { signalDisconnect handler
                ; entryGetText input >>= next } }
        }

    entrySetText guess         $ datas !! 0
    entrySetText mostLikely    $ datas !! 1
    entrySetText sndMostLikely $ datas !! 2
    entrySetText expectedInfo  $ datas !! 3

endPhase :: Builder -> String -> IO ()
endPhase builder text = do
    enterWindow <- builderGetObject builder castToWindow "enterWindow"
    endWindow   <- builderGetObject builder castToWindow "endWindow"
    gameGrid    <- builderGetObject builder castToGrid "gameGrid"
    endGrid     <- builderGetObject builder castToGrid "endGrid"

    containerRemove enterWindow gameGrid
    containerRemove endWindow endGrid
    containerAdd enterWindow endGrid

    solved <- builderGetObject builder castToEntry "solved"
    entrySetText solved text