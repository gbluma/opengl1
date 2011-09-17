import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GL
import Bindings
import Display
import Data.IORef

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = 
  [(0.2, 0.3, 0.0),
   (0.5, 0.2, 0.0),
   (0.5, 0.5, 0.0)]


-------------------------------------------------------------
main = do
  (progname, _) <- getArgsAndInitialize


  -- state variables, holy cow!
  angle    <- newIORef (0.0::GLfloat)
  delta    <- newIORef (0.1::GLfloat)
  position <- newIORef (0.0::GLfloat, 0.0)


  window <- initGL
  GL.depthFunc $= Just Less

  -- register a display callback (found in Display.hs)
  GL.displayCallback $= (display angle position)

  -- register a reshape callback (found in Bindings.hs)
  GL.reshapeCallback $= Just reshape
  
  -- setup keyboard and mouse (found in Bindings.hs)
  GL.keyboardMouseCallback $= Just (keyboardMouse window delta position)

  GL.idleCallback $= Just (idle angle delta)
  
  GL.mainLoop


