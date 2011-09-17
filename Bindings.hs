module Bindings (idle,display, reshape, keyboardMouse) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Display

-------------------------------------------------------------
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)

-------------------------------------------------------------
keyboardAct delta position (Char ' ') Down = do
  -- flip the direction of rotation
  delta' <- get delta
  delta $= -delta'

-------------------------------------------------------------
keyboardAct delta position (Char '=') Down = do
  -- speed up rotation
  delta' <- get delta
  delta $= 2*delta'

-------------------------------------------------------------
keyboardAct delta position (Char '-') Down = do
  -- slow down rotation
  delta' <- get delta
  delta $= -delta'/2

-------------------------------------------------------------
keyboardAct delta position (SpecialKey KeyLeft) Down = do
  -- move left (reduce x component)
  (x,y) <- get position
  position $= (x-0.1,y)

-------------------------------------------------------------
keyboardAct delta position (SpecialKey KeyRight) Down = do
  -- move right (boost x component)
  (x,y) <- get position
  position $= (x-0.1,y)

-------------------------------------------------------------
keyboardAct delta position (SpecialKey KeyUp) Down = do
  -- move up (boost y component)
  (x,y) <- get position
  position $= (x,y+0.1)

-------------------------------------------------------------
keyboardAct delta position (SpecialKey KeyDown) Down = do
  -- move down (reduce y component)
  (x,y) <- get position
  position $= (x,y-0.1)

-------------------------------------------------------------
keyboardAct _ _ _ _ = return () 

keyboardMouse angle pos key state modifiers position = do
  keyboardAct angle pos key state

