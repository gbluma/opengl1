module GameState where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT (Window, elapsedTime)
import Data.IORef (IORef, newIORef)

-------------------------------------------------------------
data GameState = GameState { 
  angle    :: IORef (GLfloat),
  delta    :: IORef (GLfloat),
  pos      :: IORef (GLfloat, GLfloat, GLfloat),
  time     :: IORef (Int),
  fps      :: IORef (Int)
}

-------------------------------------------------------------
makeGameState :: IO GameState
makeGameState = do
  -- initialize values
  angle'    <- newIORef (0.0::GLfloat) 
  delta'    <- newIORef (0.1::GLfloat)
  pos'      <- newIORef (0.0::GLfloat, 0.0, 0.0)
  time'     <- newIORef (0::Int)
  fps'      <- newIORef (0::Int)
   
  return $ GameState {
    angle    = angle',
    delta    = delta',
    pos      = pos',
    time     = time',
    fps      = fps'
  }

-------------------------------------------------------------
updateFPS :: GameState -> IO ()
updateFPS gameState = do

  -- TODO: move FPS code inside GameState module
  prevTime <- get (time gameState)
  currTime <- get (elapsedTime)
  let diff =  (fromIntegral (currTime - prevTime))
  time gameState $= currTime
  fps  gameState $= truncate (1000.0 / diff)

