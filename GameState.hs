module GameState where

import Graphics.UI.GLUT
import Data.IORef (IORef, newIORef)
import Textures 
import GameObject

-------------------------------------------------------------
data GameState = GameState { 
  angle    :: IORef GLfloat,
  delta    :: IORef GLfloat,
  pos      :: IORef (GLfloat, GLfloat, GLfloat),
  time     :: IORef Int,
  fps      :: IORef Int,
  textures :: IORef [Maybe TextureObject],
  gameObject :: IORef GameObject
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
  
  gameObject' <- makeGameObject
  gameObject'' <- newIORef gameObject'

  -- TODO: there should be a way to combine these two lines
  textures' <- getAndCreateTextures []
  textures'' <- newIORef textures'

  return $ GameState {
    angle    = angle',
    delta    = delta',
    pos      = pos',
    time     = time',
    fps      = fps',
    textures = textures'',
    gameObject = gameObject''
  }

-------------------------------------------------------------
updateFPS :: GameState -> IO ()
updateFPS gameState = do

  -- calcualte the differnece in time
  prevTime <- get (time gameState)
  currTime <- get (elapsedTime)
  let diff =  (fromIntegral (currTime - prevTime))
  let fps' = truncate (1000.0 / diff)

  -- update the global game state
  time gameState $= currTime
  fps  gameState $= fps' 

  -- output the fps for debugging
  -- putStrLn $ show fps'
