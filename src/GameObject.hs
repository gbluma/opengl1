
module GameObject where

import Graphics.UI.GLUT 
--import Graphics.GLUtil
import Data.IORef
import Textures 

-------------------------------------------------------------
data GameObject = GameObject { 
  location      :: IORef (GLfloat, GLfloat, GLfloat)
  --rotation :: IORef (GLfloat, GLfloat, GLfloat),
  --textures :: IORef [Maybe TextureGameObject]
}


makeGameObject :: IO GameObject
makeGameObject = do
  -- initilize parameters for new object
  location'      <- newIORef (0.0::GLfloat, 0.0, (-1.0) )

  -- create and return
  return $ GameObject {
    location      = location'
  }

