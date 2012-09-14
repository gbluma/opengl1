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
  let delta' = delta gameState
  return $ gameState { delta = -delta' }

-------------------------------------------------------------
keyboardAct _ gameState (Char '=') Down = do
  -- speed up rotation
  let delta' = delta gameState
  return $ gameState { delta = 2*delta' }

-------------------------------------------------------------
keyboardAct _ gameState (Char '-') Down = do
  -- slow down rotation
  let delta' = delta gameState
  return $ gameState { delta = delta'/2 }

-------------------------------------------------------------
keyboardAct _ gameState (SpecialKey KeyLeft) Down = do
  -- move left (reduce x component)
  let (x,y,z) = pos gameState
  return $ gameState { pos = (x-0.02,y,z) }

-------------------------------------------------------------
keyboardAct _ gameState (SpecialKey KeyRight) Down = do
  -- move right (boost x component)
  let (x,y,z) = pos gameState
  return $ gameState { pos = (x+0.02,y,z) }

-------------------------------------------------------------
keyboardAct _ gameState (SpecialKey KeyUp) Down = do
  -- move up (boost y component)
  let (x,y,z) = pos gameState
  return $ gameState { pos = (x,y+0.02,z) }

-------------------------------------------------------------
keyboardAct _ gameState (SpecialKey KeyDown) Down = do
  -- move down (reduce y component)
  let (x,y,z) = pos gameState
  return $ gameState { pos = (x,y-0.02,z) }

-------------------------------------------------------------
keyboardAct _ gameState (Char 'z') Down = do
  -- move up (boost z component)
  let (x,y,z) = pos gameState
  return $ gameState { pos = (x,y,z+0.02) }

-------------------------------------------------------------
keyboardAct _ gameState (Char 'x') Down = do
  -- move down (reduce z component)
  let (x,y,z) = pos gameState
  return $ gameState { pos = (x,y,z-0.02) }

-------------------------------------------------------------
keyboardAct window gameState (Char '\ESC') Down = do
  -- Exit via escape button
  destroyWindow window
  exitWith ExitSuccess
  return $ gameState

-------------------------------------------------------------
keyboardAct _ gameState _ _ = return gameState

keyboardMouse window gameState key state modifiers position = do
  keyboardAct window gameState key state
  return gameState

