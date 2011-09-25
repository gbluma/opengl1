module Cube where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


drawFace :: Normal3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
         -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
drawFace p q r s t = do
   let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
   normal p
   texCoord2f (TexCoord2 1 1)
   vertex q
   texCoord2f (TexCoord2 0 1)
   vertex r
   texCoord2f (TexCoord2 0 0)
   vertex s
   texCoord2f (TexCoord2 1 0)
   vertex t

drawCube :: GLfloat -> IO ()
drawCube size = do
   let
       a = Vertex3   size    size    size
       b = Vertex3   size    size  (-size)
       c = Vertex3   size  (-size) (-size)
       d = Vertex3   size  (-size)   size
       e = Vertex3 (-size)   size    size
       f = Vertex3 (-size)   size  (-size)
       g = Vertex3 (-size) (-size) (-size)
       h = Vertex3 (-size) (-size)   size

       i = Normal3   1    0    0
       k = Normal3 (-1)   0    0
       l = Normal3   0    0  (-1)
       m = Normal3   0    0    1
       n = Normal3   0    1    0
       o = Normal3   0  (-1)   0

   renderPrimitive Quads $ do
      drawFace i d c b a
      drawFace k g h e f
      drawFace l c g f b
      drawFace m h d a e
      drawFace n e a b f
      drawFace o g c d h

