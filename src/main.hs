import Graphics.UI.GLUT
import Data.IORef (newIORef)

import Bindings
import Display
import GameState

-------------------------------------------------------------
main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize

  gameState <- makeGameState

  (window, gameState') <- initGL gameState
  depthFunc $= Just Less

  -- fullScreen

  -- TODO: 
  --  * rewrite these functions to be recursive instead of callback-based
  --  * or switch to GLFW

  -- register a display callback (found in Display.hs)
  displayCallback $= (display gameState')

  -- register a reshape callback (found in Bindings.hs)
  -- reshapeCallback $= Just reshape
  
  -- setup keyboard and mouse (found in Bindings.hs)
  -- keyboardMouseCallback $= Just (keyboardMouse window gameState')

  -- idleCallback $= Just (idle gameState')


  -- enter infinite loop
  mainLoop


