module GameState where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import Textures
import GameObject
import Shaders

-------------------------------------------------------------
data GameState = GameState { 
  angle       :: GLfloat,
  delta       :: GLfloat,
  pos         :: (GLfloat, GLfloat, GLfloat),
  time        :: Double,
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
                     , time     = 0.0
                     , fps      = 0 
                     , textures = textures' 
                     , gameObject = gameObject' }

