module Bindings (idle,display, reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Display
import System.Exit

import GameState

-------------------------------------------------------------
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  
  matrixMode $= Projection
  loadIdentity
  perspective 45 ((fromIntegral w)/(fromIntegral h)) 0.02 130
  matrixMode $= Modelview 0

-------------------------------------------------------------
keyboardAct _ gameState (Char ' ') Down = do
  -- flip the direction of rotation
  delta' <- get (delta gameState)
  (delta gameState) $= -delta'

-------------------------------------------------------------
keyboardAct _ gameState (Char '=') Down = do
  -- speed up rotation
  delta' <- get (delta gameState)
  (delta gameState) $= 2*delta'

-------------------------------------------------------------
keyboardAct _ gameState (Char '-') Down = do
  -- slow down rotation
  delta' <- get (delta gameState)
  (delta gameState) $= delta'/2

-------------------------------------------------------------
keyboardAct _ gameState (SpecialKey KeyLeft) Down = do
  -- move left (reduce x component)
  (x,y,z) <- get (pos gameState)
  (pos gameState) $= (x-0.02,y,z)

-------------------------------------------------------------
keyboardAct _ gameState (SpecialKey KeyRight) Down = do
  -- move right (boost x component)
  (x,y,z) <- get (pos gameState)
  (pos gameState) $= (x+0.02,y,z)

-------------------------------------------------------------
keyboardAct _ gameState (SpecialKey KeyUp) Down = do
  -- move up (boost y component)
  (x,y,z) <- get (pos gameState)
  (pos gameState) $= (x,y+0.02,z)

-------------------------------------------------------------
keyboardAct _ gameState (SpecialKey KeyDown) Down = do
  -- move down (reduce y component)
  (x,y,z) <- get (pos gameState)
  (pos gameState) $= (x,y-0.02,z)

-------------------------------------------------------------
keyboardAct _ gameState (Char 'z') Down = do
  -- move up (boost z component)
  (x,y,z) <- get (pos gameState)
  (pos gameState) $= (x,y,z+0.02)

-------------------------------------------------------------
keyboardAct _ gameState (Char 'x') Down = do
  -- move down (reduce z component)
  (x,y,z) <- get (pos gameState)
  (pos gameState) $= (x,y,z-0.02)

-------------------------------------------------------------
keyboardAct window _ (Char '\ESC') Down = do
  -- Exit via escape button
  destroyWindow window
  exitWith ExitSuccess

-------------------------------------------------------------
keyboardAct _ _ _ _ = return () 

keyboardMouse window gameState key state modifiers position = do
  keyboardAct window gameState key state

