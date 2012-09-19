module GameState where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import Textures
import GameObject
import Shaders

-------------------------------------------------------------
data Status = Status_Init
            | Status_Shutdown
            deriving (Show, Eq)

-------------------------------------------------------------
data Camera = Camera {
  x     :: GLfloat,
  y     :: GLfloat,
  z     :: GLfloat,
  yaw   :: GLfloat,
  pitch :: GLfloat
}

-------------------------------------------------------------
data GameState = GameState { 
  angle       :: GLfloat,
  delta       :: GLfloat,
  pos         :: (GLfloat, GLfloat, GLfloat),
  camera      :: Camera,
  time        :: Double,
  fps         :: Int,
  textures    :: [Maybe TextureObject],
  gameObject  :: GameObject,
  gameStatus  :: Status
}

-------------------------------------------------------------
makeGameState :: IO GameState
makeGameState = do

  gameObject' <- makeGameObject
  textures' <- getAndCreateTextures []

  return $ GameState { angle    = 0.0 
                     , delta    = 0.1 
                     , pos      = (0.0,0.0,0.0) 
                     , camera   = Camera 0 0 (-1.0) 0 0
                     , time     = 0.0
                     , fps      = 0 
                     , textures = textures' 
                     , gameObject = gameObject'
                     , gameStatus = Status_Init}

