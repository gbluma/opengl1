import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Bindings

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = 
  [(0.2, 0.3, 0.0),
   (0.5, 0.2, 0.0),
   (0.5, 0.5, 0.0)]


-------------------------------------------------------------
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"

  -- register a display callback (found in Display.hs)
  displayCallback $= display

  -- register a reshape callback (found in Bindings.hs)
  reshapeCallback $= Just reshape
  
  mainLoop

