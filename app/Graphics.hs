module Graphics (
                enterPhase
              , gamePhase
              , setGameDisplay)
                where

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

enterPhaseDisplay :: IO Entry
enterPhaseDisplay = do
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

enterPhase :: (Window -> String -> IO ()) -> IO ()
enterPhase readyFunc = do
    _ <- initGUI
    window <- renderWindow
    _ <- window `on` deleteEvent $ do
        liftIO mainQuit
        return False
    display <- enterPhaseDisplay

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
        entryGetText display >>= readyFunc window . parse
        ) >>= attach 2 1 1 1

    containerAdd window grid
    widgetShowAll window
    mainGUI


gamePhaseDisplay :: IO Entry
gamePhaseDisplay = do
    display <- entryNew
    set display [ entryEditable := False
                , entryXalign   := 0.5
                , entryText     := "" ]
    pure display

setGameDisplay :: Entry -> [String] -> IO ()
setGameDisplay display datas = entrySetText display $ datas !! 0

gamePhase :: Window -> IO Entry
gamePhase window = do
    display <- gamePhaseDisplay

    grid <- gridNew
    gridSetRowHomogeneous grid True
    let attach x y w h item = gridAttach grid item x y w h

    attach 0 0 3 1 display

    containerAdd window grid
    widgetShowAll window
    pure display