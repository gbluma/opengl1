module Bindings (idle,display,reshape,keyboardAct) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit

import Display
import GameState

-------------------------------------------------------------
reshape s@(Size w h) = do
  putStrLn $ "Resizing to: " ++ (show w) ++ ", " ++ (show h)
  viewport $= (Position 0 0, s)
  
  matrixMode $= Projection
  loadIdentity 
  perspective 45 ((fromIntegral w)/(fromIntegral h)) 0.02 130
  matrixMode $= Modelview 0

-------------------------------------------------------------
keyboardAct           :: GameState -> IO GameState
keyboardAct gameState = (return gameState) 
                          >>= handleEsc
                          >>= handleSpace
                          >>= handleEqual >>= handleMinus
                          >>= handleLeft >>= handleRight >>= moveForward >>= moveBackward

-------------------------------------------------------------
handleEsc           :: GameState -> IO GameState
handleEsc gameState = do
  key <- GLFW.getKey GLFW.ESC
  return $ case key of
    GLFW.Press   -> gameState { gameStatus = Status_Shutdown }
    GLFW.Release -> gameState  -- pass thru
  
-------------------------------------------------------------
handleEqual           :: GameState -> IO GameState
handleEqual gameState = do
  key <- GLFW.getKey '='
  return $ case key of
    GLFW.Press   -> gameState { delta = (1.1*(delta gameState)) }
    GLFW.Release -> gameState  -- pass thru


-------------------------------------------------------------
handleSpace           :: GameState -> IO GameState
handleSpace gameState = do
  key <- GLFW.getKey ' '
  return $ case key of
    GLFW.Press   -> gameState { delta = -(delta gameState) }
    GLFW.Release -> gameState  -- pass thru

-------------------------------------------------------------
handleMinus           :: GameState -> IO GameState
handleMinus gameState = do
  key <- GLFW.getKey '-'
  return $ case key of
    GLFW.Press   -> gameState { delta = (delta gameState)/1.1 }
    GLFW.Release -> gameState  -- pass thru

-------------------------------------------------------------
handleLeft           :: GameState -> IO GameState
handleLeft gameState = do
  key <- GLFW.getKey GLFW.LEFT
  return $ case key of
    GLFW.Press   -> do
      let Camera _ _ _ cyaw _ = camera gameState
      gameState { camera = (camera gameState) { yaw = (cyaw-0.4) } }
    GLFW.Release -> gameState  -- pass thru

-------------------------------------------------------------
handleRight           :: GameState -> IO GameState
handleRight gameState = do
  key <- GLFW.getKey GLFW.RIGHT
  return $ case key of
    GLFW.Press   -> do
      let Camera _ _ _ cyaw _ = camera gameState
      gameState { camera = (camera gameState) { yaw = (cyaw+0.4) } }
    GLFW.Release -> gameState  -- pass thru

-------------------------------------------------------------
moveForward           :: GameState -> IO GameState
moveForward gameState = do
  key <- GLFW.getKey GLFW.UP
  return $ case key of
    GLFW.Press   -> do
      let Camera cx cy cz cyaw _ = camera gameState
      gameState { 
        camera = (camera gameState) { 
          x = cx - (0.02 * sin( cyaw * (3.14159/180) )),
          z = cz + (0.02 * cos( cyaw * (3.14159/180) ))
        }}
    GLFW.Release -> gameState  -- pass thru

-------------------------------------------------------------
moveBackward           :: GameState -> IO GameState
moveBackward gameState = do
  key <- GLFW.getKey GLFW.DOWN
  return $ case key of
    GLFW.Press   -> do
      let Camera cx cy cz cyaw _ = camera gameState
      gameState { 
        camera = (camera gameState) { 
          x = cx + (0.02 * sin( cyaw * (3.14159/180) )),
          z = cz - (0.02 * cos( cyaw * (3.14159/180) ))
        }}
    GLFW.Release -> gameState  -- pass thru

