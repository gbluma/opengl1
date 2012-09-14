module GameState where

import Graphics.UI.GLUT
import Textures
import GameObject
import Shaders

-------------------------------------------------------------
data GameState = GameState { 
  angle       :: GLfloat,
  delta       :: GLfloat,
  pos         :: (GLfloat, GLfloat, GLfloat),
  time        :: Int,
  fps         :: Int,
  textures    :: [Maybe TextureObject],
  gameObject  :: GameObject
}

-------------------------------------------------------------
makeGameState :: IO GameState
makeGameState = do

  gameObject' <- makeGameObject
  textures' <- getAndCreateTextures []

  return $ GameState { angle    = 0.0 
                     , delta    = 0.1 
                     , pos      = (0.0,0.0,0.0) 
                     , time     = 0 
                     , fps      = 0 
                     , textures = textures' 
                     , gameObject = gameObject' }

-------------------------------------------------------------
updateFPS :: GameState -> IO GameState
updateFPS gameState = do

  -- calcualte the differnece in time
  let prevTime = time gameState
  currTime <- get (elapsedTime)

  let diff = fromIntegral (currTime - prevTime)
  let fps' = truncate (1000.0 / diff)

  -- output the fps for debugging
  -- putStrLn $ show fps'

  return $ gameState { time = currTime, fps  = fps' }
