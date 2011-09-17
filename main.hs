import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Bindings
import Data.IORef

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = 
  [(0.2, 0.3, 0.0),
   (0.5, 0.2, 0.0),
   (0.5, 0.5, 0.0)]


-------------------------------------------------------------
main = do
  (progname, _) <- getArgsAndInitialize

  initialDisplayMode $= [DoubleBuffered]
  -- initialWindowSize $= (Size 400 300)

  window <- createWindow "Hello World"

  -- state variables, holy cow!
  angle    <- newIORef (0.0::GLfloat)
  delta    <- newIORef (0.1::GLfloat)
  position <- newIORef (0.0::GLfloat, 0.0)

  -- register a display callback (found in Display.hs)
  displayCallback $= (display angle position)

  -- register a reshape callback (found in Bindings.hs)
  reshapeCallback $= Just reshape
  
  -- setup keyboard and mouse (found in Bindings.hs)
  keyboardMouseCallback $= Just (keyboardMouse delta position)

  idleCallback $= Just (idle angle delta)
  
  mainLoop

