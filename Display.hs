module Display (initGL,display,idle) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Control.Applicative
import Data.IORef
import TGA
import Graphics.GLUtil
import GameState

-------------------------------------------------------------
makeTexture :: FilePath -> IO TextureObject
makeTexture filename =
  -- read texture from file
  do (width, height, pixels) <- readTGA filename

     -- tranlate file data into texture
     texture <- loadTexture $ texInfo width height TexBGR pixels
     textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
     textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
     textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
     return texture



-------------------------------------------------------------
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
display :: GameState -> IO ()
display gameState = do

  

  -- clear the scene
  GLUT.clear [ GLUT.ColorBuffer, GLUT.DepthBuffer ]
  
  -- load our dynamic data
  (_, GLUT.Size xres yres) <- get GLUT.viewport
  (x,y,z) <- get (pos gameState)
  fps <- get (fps gameState)
  
  GLUT.perspective 45 ((fromIntegral xres)/(fromIntegral yres)) 0.1 100
  GLUT.matrixMode $= GLUT.Modelview 0
  GLUT.lighting $= Enabled
  GLUT.loadIdentity

  -- move the camera back one unit
  GLUT.translate $ Vector3 0 0 (-1::GLfloat) 
  
  GLUT.preservingMatrix $ do

    angle <- get (angle gameState)
    GLUT.rotate angle $ Vector3 (1::GLfloat) 0 (1::GLfloat)
    GLUT.scale 1 1 (1::GLfloat)
    
    renderAxis

    GLUT.color $ Color3 (0.3::GLfloat) (0.7::GLfloat) (0.3::GLfloat)

    -- set the tranlation
    (x,y,z) <- get (pos gameState)
    GLUT.translate $ Vector3 x y z
    
    renderObject Solid (Teapot 0.2)

  GLUT.matrixMode $= GLUT.Projection
  GLUT.loadIdentity
  GLUT.ortho2D 0 0 (fromIntegral xres) (fromIntegral yres)
  GLUT.lighting $= Disabled

  GLUT.color $ Color3 1 1 (1::GLfloat)
  GLUT.currentRasterPosition $= Vertex4  (-0.98) (0.92) 0 1
  GLUT.renderString GLUT.Fixed8By13 $  "FPS: " ++ (show fps)


  GLUT.flush
  GLUT.swapBuffers


-------------------------------------------------------------
idle :: GameState -> IO ()
idle gameState = do
  
  -- update state vars for control (todo: refactor)
  a <- get (angle gameState)
  d <- get (delta gameState)
  angle gameState $= a + d
 
  -- TODO: move this code inside GameState module
  prevTime <- get (time gameState)
  currTime <- get (GLUT.elapsedTime)
  time gameState $= currTime
  fps  gameState $= (currTime - prevTime)  
  -- TODO: for whatever reason, division by 1000 (for fps) causes this to need a monad.

  GLUT.postRedisplay Nothing


