module Graphics (enterPhase) where

import Graphics.UI.Gtk
import Control.Monad.IO.Class

renderWindow :: IO Window
renderWindow = do
    window <- windowNew
    set window [ windowTitle         := "Hangman"
               , windowResizable     := False
               , windowDefaultWidth  := 150
               , windowDefaultHeight := 100 ]
    pure window

renderDisplay :: IO Entry
renderDisplay = do
    display <- entryNew
    set display [ entryEditable := False
                , entryXalign   := 0.5
                , entryText     := "" ]
    pure display

mkBtn :: String -> IO () -> IO Button
mkBtn label func = do
    btn <- buttonNew
    set btn [ buttonLabel := label ]
    _ <- btn `on` buttonActivated $
        func
    pure btn

parse :: String -> String
parse raw = 
    let
        repl '_' = '.'
        repl c   = c
        second (x:_:xs) = x : second xs
        second _ = []
    in
        map repl (second raw)

enterPhase :: (String -> IO ()) -> IO ()
enterPhase readyFunc = do
    _ <- initGUI
    window <- renderWindow
    display <- renderDisplay

    grid <- gridNew
    gridSetRowHomogeneous grid True
    let attach x y w h item = gridAttach grid item x y w h

    attach 0 0 3 1 display
        
    mkBtn "Add Letter" (
        entryGetText display >>= entrySetText display . (++ "_ ")
        ) >>= attach 0 1 1 1
    
    mkBtn "Add Space" (
        entryGetText display >>= entrySetText display . (\orig -> 
            if not (null orig) && last (init orig) == '_' 
            then orig ++ "  " 
            else orig)
            ) >>= attach 1 1 1 1

    mkBtn "Ready" (do 
        containerRemove window grid
        entryGetText display >>= readyFunc . parse
        ) >>= attach 2 1 1 1

    containerAdd window grid

    _ <- window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    widgetShowAll window
    mainGUI