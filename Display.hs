module Display (initGL,display,idle) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import Graphics.GLUtil
import GameState
import Cube


-------------------------------------------------------------
initGL :: IO Window
initGL = do 
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  initialWindowSize $= Size 580 400
  initialWindowPosition $= Position 20 20

  initialDisplayCapabilities $= 
    [ With  DisplayRGB,
      Where DisplayDepth IsAtLeast 16,
      With  DisplaySamples,
      Where DisplayStencil IsNotLessThan 2,
      With  DisplayDouble ]

  window <- createWindow "OpenGL1"

  -- set the redraw color
  clearColor $= Color4 0.2 0.2 0.2 0

  -- tell opengl the size of the rendering area
  viewport $= (Position 20 20, Size 580 400)

  -- scale the scene to be the correct aspect ratio
  matrixMode $= Projection
  loadIdentity
  perspective 45 ((fromIntegral 580)/(fromIntegral 400)) 0.1 100
  matrixMode $= Modelview 0

  -- setup lighting
  shadeModel $= Smooth
  blend      $= Enabled 
  materialSpecular Front $= Color4 0.7 0.75 0.7 0.7
  materialShininess Front $= 127

  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 2 2 2 0
  
  -- setup texturing
  texture Texture2D $= Enabled 

  return window

-------------------------------------------------------------
renderAxis :: IO ()
renderAxis = do
  lighting $= Disabled
  renderPrimitive Lines $ do

    color  $ Color3  (1.0::GLfloat) 0 0
    vertex $ Vertex3 0 0 (0.0::GLfloat) 
    vertex $ Vertex3 0 0 (1.0::GLfloat)

    color  $ Color3  0 (1.0::GLfloat) 0
    vertex $ Vertex3 0 0 (0.0::GLfloat) 
    vertex $ Vertex3 0 (1.0::GLfloat) 0

    color  $ Color3  0 0 (1.0::GLfloat)
    vertex $ Vertex3 0 0 (0.0::GLfloat) 
    vertex $ Vertex3 (1.0::GLfloat) 0 0
  lighting $= Enabled


-------------------------------------------------------------
display :: GameState -> IO ()
display gameState = do

  -- clear the scene
  clear [ ColorBuffer, DepthBuffer ]
  
  -- load our dynamic data
  (_, Size xres yres) <- get viewport
  (x,y,z) <- get (pos gameState)
  fps <- get (fps gameState)
  
  -- perspective 45 ((fromIntegral xres)/(fromIntegral yres)) 0.1 100
  -- matrixMode $= Modelview 0
  lighting $= Enabled
  loadIdentity

  -- move the camera back one unit
  translate $ Vector3 0 0 (-1::GLfloat) 
  
  preservingMatrix $ do

    angle <- get (angle gameState)
    rotate angle $ Vector3 (1::GLfloat) 0 (1::GLfloat)
    scale 1 1 (1::GLfloat)
    
    renderAxis

    color $ Color3 (0.3::GLfloat) (0.7::GLfloat) (0.3::GLfloat)

    -- set the tranlation
    (x,y,z) <- get (pos gameState)
    translate $ Vector3 x y z
    
    renderObject Solid (Teapot 0.2)

  -- matrixMode $= Projection
  loadIdentity
  ortho2D 0 0 (fromIntegral xres) (fromIntegral yres)
  lighting $= Disabled

  color $ Color3 1 1 (1::GLfloat)
  currentRasterPosition $= Vertex4  (-0.98) (0.92) 0 1
  renderString Fixed8By13 $  "FPS: " ++ (show fps)


  flush
  swapBuffers


-------------------------------------------------------------
idle :: GameState -> IO ()
idle gameState = do
  
  -- update state vars for control (todo: refactor)
  a <- get (angle gameState)
  d <- get (delta gameState)
  angle gameState $= a + d
 
  -- update the FPS display
  updateFPS gameState

  postRedisplay Nothing


