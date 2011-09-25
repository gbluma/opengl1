import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Data.IORef (newIORef)

import Bindings
import Display


-------------------------------------------------------------
main = do
  (progname, _) <- GLUT.getArgsAndInitialize

  -- state variables, holy cow!
  angle    <- newIORef (0.0::GLfloat)
  delta    <- newIORef (0.1::GLfloat)
  position <- newIORef (0.0::GLfloat, 0.0, 0.0)

  window <- initGL
  GLUT.depthFunc $= Just Less

  -- register a display callback (found in Display.hs)
  GLUT.displayCallback $= (display angle position)

  -- register a reshape callback (found in Bindings.hs)
  GLUT.reshapeCallback $= Just reshape
  
  -- setup keyboard and mouse (found in Bindings.hs)
  GLUT.keyboardMouseCallback $= Just (keyboardMouse window delta position)

  GLUT.idleCallback $= Just (idle angle delta)

  -- enter infinite loop
  GLUT.mainLoop


