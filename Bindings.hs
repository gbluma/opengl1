module Bindings (idle,display, reshape, keyboardMouse) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GL
import Display
import System.Exit

-------------------------------------------------------------
reshape s@(Size w h) = do
  GL.viewport $= (Position 0 0, s)
  
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.perspective 45 ((fromIntegral w)/(fromIntegral h)) 0.1 130
  GL.matrixMode $= GL.Modelview 0

-------------------------------------------------------------
keyboardAct _ delta position (Char ' ') Down = do
  -- flip the direction of rotation
  delta' <- get delta
  delta $= -delta'

-------------------------------------------------------------
keyboardAct _ delta position (Char '=') Down = do
  -- speed up rotation
  delta' <- get delta
  delta $= 2*delta'

-------------------------------------------------------------
keyboardAct _ delta position (Char '-') Down = do
  -- slow down rotation
  delta' <- get delta
  delta $= delta'/2

-------------------------------------------------------------
keyboardAct _ delta position (SpecialKey KeyLeft) Down = do
  -- move left (reduce x component)
  (x,y) <- get position
  position $= (x-0.1,y)

-------------------------------------------------------------
keyboardAct _ delta position (SpecialKey KeyRight) Down = do
  -- move right (boost x component)
  (x,y) <- get position
  position $= (x-0.1,y)

-------------------------------------------------------------
keyboardAct _ delta position (SpecialKey KeyUp) Down = do
  -- move up (boost y component)
  (x,y) <- get position
  position $= (x,y+0.1)

-------------------------------------------------------------
keyboardAct _ delta position (SpecialKey KeyDown) Down = do
  -- move down (reduce y component)
  (x,y) <- get position
  position $= (x,y-0.1)

-------------------------------------------------------------
keyboardAct window _ _ (Char '\ESC') Down = do
  -- Exit via escape button
  GL.destroyWindow window
  exitWith ExitSuccess

-------------------------------------------------------------
keyboardAct _ _ _ _ _ = return () 

keyboardMouse window delta pos key state modifiers position = do
  keyboardAct window delta pos key state

