import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Data.IORef (newIORef)

import Bindings
import Display
import GameState

-------------------------------------------------------------
main = do
  (progname, _) <- GLUT.getArgsAndInitialize

  gameState <- makeGameState

  window <- initGL
  GLUT.depthFunc $= Just Less

  -- register a display callback (found in Display.hs)
  GLUT.displayCallback $= (display gameState)

  -- register a reshape callback (found in Bindings.hs)
  GLUT.reshapeCallback $= Just reshape
  
  -- setup keyboard and mouse (found in Bindings.hs)
  GLUT.keyboardMouseCallback $= Just (keyboardMouse window gameState)

  GLUT.idleCallback $= Just (idle gameState)

  -- enter infinite loop
  GLUT.mainLoop


