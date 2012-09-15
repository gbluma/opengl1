
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)

import Bindings
import Display
import GameState

-------------------------------------------------------------
gameLoop gameState = do

  -- render
  gameState' <- display gameState
  GLFW.swapBuffers

  GLFW.sleep 0.01
  
  -- update game world
  gameState'' <- idle gameState'

  -- TODO: rewrite key/mouse controls here

  -- only continue displaying if window is open
  windowOpen <- getParam Opened
  unless (not windowOpen) $
    gameLoop gameState''

-------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName

  gameState <- makeGameState
  gameState' <- initGL $ gameState

  -- TODO: move reshape out of Bindings.hs
  GLFW.windowSizeCallback $= reshape

  -- enter infinite loop
  gameLoop gameState'

  -- probably never actually get here, but in case we do...
  GLFW.closeWindow
  GLFW.terminate


