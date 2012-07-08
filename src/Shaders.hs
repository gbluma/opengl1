module Shaders where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Shaders (VertexShader, FragmentShader, Program)
import Graphics.Rendering.OpenGL.Raw.Core31

import Foreign.Ptr (Ptr)
import Control.Monad (unless)
import Unsafe.Coerce (unsafeCoerce)
import System.IO (hPutStrLn, stderr)
import Control.Applicative


data SSAO_Shader = SSAO_Shader 
  { vertShaders :: VertexShader
  , fragShaders :: FragmentShader
  , program     :: Program
  , rnm         :: AttribLocation
  , normalMap   :: AttribLocation
  } 

-- actually, this is probably going to be scrapped. Too much work for the small benefit
initSSAO = do
  vs <- loadShader "shaders/ssao.vert"
  fs <- loadShader "shaders/ssao.frag"
  p  <- linkShaderProgram [vs] [fs]
  SSAO_Shader vs fs p
    <$> get (attribLocation p "rnm")                 -- TODO: Actually pipe the depth map in here (At least I think it's the depth map)
    <*> get (attribLocation p "normalMap")           -- TODO: Also pipe in the normal map here.


-- |Check OpenGL error flags and print them on 'stderr'.
checkErrors :: IO ()
checkErrors = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

-- |Load a shader program from a file.
loadShader :: Shader s => FilePath -> IO s
loadShader filePath = do
  src <- readFile filePath
  [shader] <- genObjectNames 1
  shaderSource shader $= [src]
  compileShader shader
  checkErrors
  ok <- get (compileStatus shader)
  infoLog <- get (shaderInfoLog shader)
  unless (null infoLog)
         (mapM_ putStrLn 
                ["Shader info log for '" ++ filePath ++ "':", infoLog, ""])
  unless ok $ do
    deleteObjectNames [shader]
    ioError (userError "shader compilation failed")
  return shader

-- |Link vertex and fragment shaders into a 'Program'.
linkShaderProgram :: [VertexShader] -> [FragmentShader] -> IO Program
linkShaderProgram vs fs = do
  [prog] <- genObjectNames 1
  attachedShaders prog $= (vs, fs)
  linkProgram prog
  checkErrors
  ok <- get (linkStatus prog)
  infoLog <- get (programInfoLog prog)
  unless (null infoLog)
         (mapM_ putStrLn ["Program info log:", infoLog, ""])
  unless ok $ do
    deleteObjectNames [prog]
    ioError (userError "GLSL linking failed")
  return prog

-- |Work with a named uniform shader parameter. Note that this looks
-- up the variable name on each access, so uniform parameters that
-- will be accessed frequently should instead be resolved to a
-- 'UniformLocation'.
namedUniform :: (Uniform a) => String -> StateVar a
namedUniform name = makeStateVar (loc >>= get) (\x -> loc >>= ($= x))
  where loc = do Just p <- get currentProgram
                 l <- get (uniformLocation p name)
                 checkErrors
                 return $ uniform l

-- Allocate an OpenGL matrix from a nested list matrix, and pass a
-- pointer to that matrix to an 'IO' action.
withHMatrix :: [[GLfloat]] -> (Ptr GLfloat -> IO a) -> IO a
withHMatrix lstMat m = do
    mat <- newMatrix RowMajor (concat lstMat) :: IO (GLmatrix GLfloat)
    withMatrix mat (\_ -> m)

-- Not all raw uniform setters are wrapped by the OpenGL interface,
-- but the UniformLocation newtype is still helpful for type
-- discipline.
unUL :: UniformLocation -> GLint
unUL = unsafeCoerce

-- |Set a 'UniformLocation' from a list representation of a
-- low-dimensional vector of 'GLfloat's. Only 2, 3, and 4 dimensional
-- vectors are supported.
uniformVec :: UniformLocation -> SettableStateVar [GLfloat]
uniformVec loc = makeSettableStateVar aux
  where aux [x,y] = glUniform2f loc' x y
        aux [x,y,z] = glUniform3f loc' x y z
        aux [x,y,z,w] = glUniform4f loc' x y z w
        aux _ = ioError . userError $
                "Only 2, 3, and 4 dimensional vectors are supported"
        loc' = unUL loc

-- |Set a named uniform shader parameter from a nested list matrix
-- representation. Only 3x3 and 4x4 matrices are supported.
namedUniformMat :: String -> SettableStateVar [[GLfloat]]
namedUniformMat var = makeSettableStateVar (\m -> loc >>= ($= m) . uniformMat)
  where loc = do Just p <- get currentProgram
                 location <- get (uniformLocation p var)
                 checkErrors
                 return location

-- |Set a uniform shader location from a nested list matrix
-- representation. Only 3x3 and 4x4 matrices are supported.
uniformMat :: UniformLocation -> SettableStateVar [[GLfloat]]
uniformMat loc = makeSettableStateVar aux
  where aux mat = do withHMatrix mat $ \ptr ->
                       case length mat of
                         4 -> glUniformMatrix4fv loc' 1 1 ptr
                         3 -> glUniformMatrix3fv loc' 1 1 ptr
                         _ -> ioError . userError $ 
                              "Only 3x3 and 4x4 matrices are supported"
        loc' = unUL loc

-- |Set a uniform shader location with a 4x4 'GLmatrix'.
uniformGLMat4 :: UniformLocation -> SettableStateVar (GLmatrix GLfloat)
uniformGLMat4 loc = makeSettableStateVar aux
  where aux m = withMatrix m $ \_ -> glUniformMatrix4fv loc' 1 1
        loc' = unUL loc

