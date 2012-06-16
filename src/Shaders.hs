module Shaders where

import Data.IORef (IORef, newIORef)

import Graphics.Rendering.OpenGL.GL.Shaders
import Graphics.GLUtil

data SSAO_Shader = SSAO_Shader {
  vertShaders :: VertexShader,
  fragShaders :: FragmentShader,
  programs    :: Program
} 

initSSAO = do
  vs <- loadShader "shaders/ssao.vert"
  fs <- loadShader "shaders/ssao.frag"
  p  <- linkShaderProgram [vs] [fs]
  return $ SSAO_Shader vs fs p

