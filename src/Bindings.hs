module Bindings (idle,display, reshape,keyboardAct) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
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
keyboardAct gameState = do
  esc <- GLFW.getKey GLFW.ESC
  --speed_up <- GLFW.getKey '='

  return $ case esc of
    GLFW.Press   -> gameState { gameStatus = Status_Shutdown }
    GLFW.Release -> gameState

  -- TODO: consider using applicatives to chain up these keyboard rules

  -- case speed_up of
  --   GLFW.Press   -> gameState { delta = (2.0*(delta gameState)) }
  --   GLFW.Release -> gameState


-- -------------------------------------------------------------
-- keyboardAct gameState (Char ' ') Down = do
--   -- flip the direction of rotation
--   let delta' = delta gameState
--   return $ gameState { delta = -delta' }
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState (Char '=') Down = do
--   -- speed up rotation
--   let delta' = delta gameState
--   return $ gameState { delta = 2*delta' }
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState (Char '-') Down = do
--   -- slow down rotation
--   let delta' = delta gameState
--   return $ gameState { delta = delta'/2 }
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState (SpecialKey KeyLeft) Down = do
--   -- move left (reduce x component)
--   let (x,y,z) = pos gameState
--   return $ gameState { pos = (x-0.02,y,z) }
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState (SpecialKey KeyRight) Down = do
--   -- move right (boost x component)
--   let (x,y,z) = pos gameState
--   return $ gameState { pos = (x+0.02,y,z) }
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState (SpecialKey KeyUp) Down = do
--   -- move up (boost y component)
--   let (x,y,z) = pos gameState
--   return $ gameState { pos = (x,y+0.02,z) }
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState (SpecialKey KeyDown) Down = do
--   -- move down (reduce y component)
--   let (x,y,z) = pos gameState
--   return $ gameState { pos = (x,y-0.02,z) }
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState (Char 'z') Down = do
--   -- move up (boost z component)
--   let (x,y,z) = pos gameState
--   return $ gameState { pos = (x,y,z+0.02) }
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState (Char 'x') Down = do
--   -- move down (reduce z component)
--   let (x,y,z) = pos gameState
--   return $ gameState { pos = (x,y,z-0.02) }
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState (Char '\ESC') Down = do
--   -- Exit via escape button
--   GLFW.closeWindow
--   GLFW.terminate
--   exitWith ExitSuccess
--   return $ gameState
-- 
-- -------------------------------------------------------------
-- keyboardAct gameState _ _ = return gameState
-- 
-- keyboardMouse gameState key state modifiers position = do
--   keyboardAct gameState key state
--   return gameState
-- 
