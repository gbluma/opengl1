module Display (initGL,display,idle) where

import Graphics.UI.GLUT hiding (Program, currentProgram)
import Graphics.Rendering.OpenGL.GL.Shaders (Program, currentProgram, VertexShader)
import Control.Applicative
import Data.IORef
-- import Graphics.GLUtil
import GameState
import Cube
import Textures
import GameObject

-------------------------------------------------------------
initGL:: GameState -> IO (Window, GameState)
initGL gameState = do 

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
  clearColor $= Color4 0.16 0.15 0.15 0

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
  light (Light 1) $= Enabled
  position (Light 1) $= Vertex4 (-2) 2 2 0
  
  -- setup texturing
  texture Texture2D $= Enabled 
  texs <- getAndCreateTextures ["blocks11b", "test"]

  let new_gamestate = gameState { textures = texs }

  return (window, new_gamestate)

-------------------------------------------------------------
renderAxis :: IO ()
renderAxis = do
  -- disable lighting/texturing for bright lines
  lighting $= Disabled
  texture Texture2D $= Disabled

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

  -- re-enable texturing and lighting now.
  lighting $= Enabled
  texture Texture2D $= Enabled 


-------------------------------------------------------------
renderFPS :: Int-> Int -> Int -> IO ()
renderFPS fps screenWidth screenHeight = do
  -- disable lighting/texturing for bright color
  lighting $= Disabled
  texture Texture2D $= Disabled

  -- 20 pixels from the edge
  let x_margin = 20.0
  let y_margin = 20.0

  -- assumes bottom left 
  let x_pos = (-1.0) + (x_margin / (fromIntegral screenWidth) )
  let y_pos = (-1.0) + (y_margin / (fromIntegral screenHeight) )

  -- do rendering 
  color $ Color3 1 1 (1::GLfloat)
  currentRasterPosition $= Vertex4 x_pos y_pos 0 1
  renderString Fixed8By13 $  "FPS: " ++ (show fps)
  
  -- re-enable texturing and lighting now.
  lighting $= Enabled
  texture Texture2D $= Enabled 


  -------------------------------------------------------------
display :: GameState -> IO ()
display gameState = do

  -- clear the scene
  clear [ ColorBuffer, DepthBuffer ]
  
  -- load our dynamic data
  (_, Size xres yres) <- get viewport
  let (x,y,z)   = pos gameState
  let _fps      = fps gameState
  let _textures = textures gameState
  
  perspective 45 ((fromIntegral xres)/(fromIntegral yres)) 0.1 100
  matrixMode $= Modelview 0
  
  lighting $= Enabled
  loadIdentity

  -- move the camera back one unit
  translate $ Vector3 0 0 (-1::GLfloat) 
  
  preservingMatrix $ do

    let _angle = angle gameState
    rotate _angle $ Vector3 (1::GLfloat) 0 (1::GLfloat)
    scale 1 1 (1::GLfloat) 
    
    renderAxis

    color $ Color3 (0.3::GLfloat) (0.7::GLfloat) (0.3::GLfloat)

    -- set the tranlation
    -- (x,y,z) <- get (pos gameState)
    translate $ Vector3 x y z
    
    textureBinding Texture2D $= (_textures !! 0)
    renderObject Solid (Teapot 0.2)
    
    --textureBinding Texture2D $= (_textures !! 1)
    -- textureBinding Texture2D $= Just t1

    translate $ Vector3 (0.5::GLfloat) y z
    -- TODO: set the texture
    drawCube 0.2

    -- draw room
    drawInvertedCube 6.0

  renderGameObject gameState $ gameObject gameState

  matrixMode $= Projection 
  loadIdentity
  ortho2D 0 0 (fromIntegral xres) (fromIntegral yres)
  renderFPS _fps (fromIntegral xres) (fromIntegral yres)

  flush
  swapBuffers


-------------------------------------------------------------
idle :: GameState -> IO GameState
idle gameState = do
  
  -- update state vars for control (todo: refactor)
  let a = angle gameState
  let d = delta gameState

  -- calculate fps
  let prevTime = time gameState
  currTime <- get (elapsedTime)
  let diff = fromIntegral (currTime - prevTime)
  let fps' = truncate (1000.0 / diff)

  postRedisplay Nothing

  return $ gameState { time = currTime
                     , fps  = fps'
                     , angle = a + d }
 




-------------------------------------------------------------
renderGameObject :: GameState -> GameObject -> IO ()
renderGameObject gameState gameObject = do
  
  let _textures = textures gameState
  (x,y,z) <- get (location gameObject)

  preservingMatrix $ do

    let _angle = angle gameState
    rotate _angle $ Vector3 (1::GLfloat) 0 (1::GLfloat)
    scale 1 1 (1::GLfloat)
    
    --renderAxis

    color $ Color3 (0.3::GLfloat) (0.7::GLfloat) (0.3::GLfloat)

    -- set the tranlation
    translate $ Vector3 x y z
    
    textureBinding Texture2D $= (_textures !! 0)
    renderObject Solid (Teapot 0.2)