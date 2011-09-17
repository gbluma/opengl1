module Display (initGL,display,idle) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GL
import Data.IORef
import TGA
import Graphics.GLUtil

--vertex4 :: Float -> Float -> Float -> Float -> GL.Vertex4 Float
--vertex4 = GL.Vertex4 

initGL = do 
  GL.initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  GL.initialWindowSize $= GL.Size 580 400
  GL.initialWindowPosition $= GL.Position 20 20

  GL.initialDisplayCapabilities $= 
    [ With  DisplayRGB,
      Where DisplayDepth IsAtLeast 16,
      With  DisplaySamples,
      Where DisplayStencil IsNotLessThan 2,
      With  DisplayDouble ]

  window <- GL.createWindow "OpenGL1"

  -- set the redraw color
  GL.clearColor $= GL.Color4 0.2 0.2 0.2 0

  -- tell opengl the size of the rendering area
  GL.viewport $= (GL.Position 20 20, GL.Size 580 400)

  -- scale the scene to be the correct aspect ratio
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.perspective 45 ((fromIntegral 580)/(fromIntegral 400)) 0.1 100
  GL.matrixMode $= GL.Modelview 0

  return window

-------------------------------------------------------------
display angle position = do

  -- clear the scene
  GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]

  -- provide a relative rendering context
  GL.loadIdentity

  a <- get angle
  GL.translate $ Vector3 (0.0::GLfloat) (0.0::GLfloat) (-1::GLfloat)
  GL.rotate a $ Vector3 (1::GLfloat) 0 (1::GLfloat)
  GL.scale 1 1 (1::GLfloat)

  GL.preservingMatrix $ do

    -- set the color
    GL.color $ Color3 (0.3::GLfloat) (0.7::GLfloat) (0.3::GLfloat)

    -- set the tranlation
    GL.translate $ Vector3 (0.0::GLfloat) (0.0::GLfloat) (-0.0::GLfloat)


    -- finally apply the mesh to the above transforms
    -- cube (0.2::GLfloat)
    renderObject Solid (Teapot 0.2)

{-
  GL.translate $ Vector3 (0.5::GLfloat) 0.5 0
  GL.color $ Color3 1 1 (1::GLfloat)
-}
  --GL.currentRasterPosition $= Vertex4 0 0 0 1 
  GL.renderString GL.Fixed8By13 "Testing" 


  -- display the scene
  GL.swapBuffers


-------------------------------------------------------------
idle angle delta = do
  
  -- update state vars for control (todo: refactor)
  a <- get angle
  d <- get delta
  angle $= a + d
  
  -- get new time (to calculate FPS)
  {--tnew <- get GL.elapsedTime
  tdiff <- tnew - told
  told <- tnew

  print tdiff
--}
  GL.postRedisplay Nothing


