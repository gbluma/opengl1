module Display (display) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Cube
import Data.IORef

-------------------------------------------------------------
display = do
  -- clear the scene
  clear [ColorBuffer]

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
  flush

