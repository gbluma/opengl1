module Display (display,idle) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Cube
import Data.IORef

-------------------------------------------------------------
display angle = do
  -- clear the scene
  clear [ColorBuffer]

  loadIdentity

  a <- get angle
  rotate a $ Vector3 0 0 (1::GLfloat)
  scale 0.7 0.7 (0.7::GLfloat)

  -- introduce a context matrix
  preservingMatrix $ do

    -- set the color
    color $ Color3 (0.3::GLfloat) 
                   (0.7::GLfloat) 
                   (0.3::GLfloat)

    -- set the tranlation
    translate $ Vector3 (0.3::GLfloat)
                        (0.2::GLfloat)
                        (0.0::GLfloat)


    -- finally apply the mesh to the above transforms
    cube (0.2::GLfloat)

  -- display the scene
  swapBuffers


-------------------------------------------------------------
idle angle = do
  a <- get angle
  angle $= a + 0.1
  postRedisplay Nothing
