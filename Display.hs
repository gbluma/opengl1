module Display (initGL,display,idle) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Data.IORef
import TGA
import Graphics.GLUtil

--vertex4 :: Float -> Float -> Float -> Float -> GLUT.Vertex4 Float
--vertex4 = GLUT.Vertex4 

initGL = do 
  GLUT.initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  GLUT.initialWindowSize $= GLUT.Size 580 400
  GLUT.initialWindowPosition $= GLUT.Position 20 20

  GLUT.initialDisplayCapabilities $= 
    [ With  DisplayRGB,
      Where DisplayDepth IsAtLeast 16,
      With  DisplaySamples,
      Where DisplayStencil IsNotLessThan 2,
      With  DisplayDouble ]

  window <- GLUT.createWindow "OpenGL1"

  -- set the redraw color
  GLUT.clearColor $= GLUT.Color4 0.2 0.2 0.2 0

  -- tell opengl the size of the rendering area
  GLUT.viewport $= (GLUT.Position 20 20, GLUT.Size 580 400)

  -- scale the scene to be the correct aspect ratio
  GLUT.matrixMode $= GLUT.Projection
  GLUT.loadIdentity
  GLUT.perspective 45 ((fromIntegral 580)/(fromIntegral 400)) 0.1 100
  GLUT.matrixMode $= GLUT.Modelview 0

  -- setup lighting
  GLUT.shadeModel $= GLUT.Smooth
  GLUT.materialSpecular Front $= Color4 0.7 0.75 0.7 0.7
  GLUT.materialShininess Front $= 127

  GLUT.lighting $= Enabled
  GLUT.light (Light 0) $= Enabled
  GLUT.position (Light 0) $= Vertex4 2 2 2 0

  return window

-------------------------------------------------------------
renderAxis = do
  GLUT.lighting $= Disabled
  renderPrimitive Lines $ do

    color  $ (Color3 (1.0::GLfloat) 0 0)
    vertex $ (Vertex3 0 0 (0::GLfloat) )
    vertex $ (Vertex3 0 0 (1.0::GLfloat))

    color  $ (Color3 0 (1.0::GLfloat) 0)
    vertex $ (Vertex3 0 0 (0::GLfloat) )
    vertex $ (Vertex3 0 (1.0::GLfloat) 0)

    color  $ (Color3 0 0 (1.0::GLfloat))
    vertex $ (Vertex3 0 0 (0::GLfloat) )
    vertex $ (Vertex3 (1.0::GLfloat) 0 0)
  GLUT.lighting $= Enabled

-------------------------------------------------------------
display angle position = do

  -- clear the scene
  GLUT.clear [ GLUT.ColorBuffer, GLUT.DepthBuffer ]

  -- provide a relative rendering context
  GLUT.loadIdentity

  a <- get angle
  GLUT.translate $ Vector3 (0.0::GLfloat) (0.0::GLfloat) (-1::GLfloat)
  GLUT.rotate a $ Vector3 (1::GLfloat) 0 (1::GLfloat)
  GLUT.scale 1 1 (1::GLfloat)

  GLUT.preservingMatrix $ do

    renderAxis

    -- set the color
    GLUT.color $ Color3 (0.3::GLfloat) (0.7::GLfloat) (0.3::GLfloat)

    -- set the tranlation
    GLUT.translate $ Vector3 (0.0::GLfloat) (0.0::GLfloat) (-0.0::GLfloat)


    -- finally apply the mesh to the above transforms
    -- cube (0.2::GLfloat)
    renderObject Solid (Teapot 0.2)

  GLUT.lighting $= Disabled
  --GLUT.matrixMode $= GLUT.Projection
  GLUT.loadIdentity
  GLUT.color $ Color3 1 1 (1::GLfloat)
  GLUT.currentRasterPosition $= Vertex4  (-0.1) 0.1 (-0.3) 1
  GLUT.renderString GLUT.Fixed8By13 $  "Tslkfjsdlfklksdlsdlksd sl dsfjld sljkdf jklsdfljkdfs ljds jlsdfjljkl ds" 

  -- display the scene
  GLUT.swapBuffers

  --GLUT.matrixMode $= GLUT.Modelview 0
  GLUT.lighting $= Enabled


-------------------------------------------------------------
idle angle delta = do
  
  -- update state vars for control (todo: refactor)
  a <- get angle
  d <- get delta
  angle $= a + d
  
  -- get new time (to calculate FPS)
  {--tnew <- get GLUT.elapsedTime
  tdiff <- tnew - told
  told <- tnew

  print tdiff
--}
  GLUT.postRedisplay Nothing


