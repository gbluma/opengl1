
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

  -- do complete processing for one frame
  -- (using monadic binds to pass gamestate, with incremental modifications, 
  --  to each of these functions)
  gameState' <- (return gameState) 
    >>= display       -- render the current frame (passing along GameState)
    >>= idle          -- handle world updating, etc. (passing along GameState)
    >>= keyboardAct   -- capture and process input (passing along GameState)

  -- check if the window is still open
  windowOpen <- getParam Opened

  -- only continue looping if window is open
  unless (not windowOpen) $
    -- check game status
    case (gameStatus gameState') of
      Status_Shutdown  -> return $ ()           -- shutting down, exit looping
      _                -> gameLoop gameState'   -- anything else, keep looping

-------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName

  -- TODO: this isn't calling correctly
  GLFW.windowSizeCallback $= reshape

  -- create and initialize GameState
  gameState <- makeGameState >>= initGL 

  -- enter infinite loop
  gameLoop gameState

  -- if we fall out of the loop, exit cleanly.
  GLFW.closeWindow
  GLFW.terminate


