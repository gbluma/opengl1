module Cube where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-------------------------------------------------------------
cube w = do
  renderPrimitive Quads $ do

    -- draw right face
    color $ (Color3 (1.0::GLfloat) 0 0)
    vertex $ (Vertex3   w    w    w )
    vertex $ (Vertex3   w    w  (-w) )
    vertex $ (Vertex3   w  (-w) (-w) )
    vertex $ (Vertex3   w  (-w)   w )

    -- draw back face
    color $ (Color3 (0::GLfloat) 0 1)
    vertex $ (Vertex3   w    w  (-w) )
    vertex $ (Vertex3 (-w)   w  (-w) )
    vertex $ (Vertex3 (-w) (-w) (-w) )
    vertex $ (Vertex3   w  (-w) (-w) )


{--
    color $ (Color3 (0::GLfloat) 1 0)
    vertex $ (Vertex3 (0::GLfloat) 0 0)
    vertex $ (Vertex3 (0::GLfloat) (-0.2) 0)
    vertex $ (Vertex3 (0.2::GLfloat) (-0.2) 0)
    vertex $ (Vertex3 (0.2::GLfloat) 0 0)
    color $ (Color3 (0::GLfloat) 0 1)
    vertex $ (Vertex3 (0::GLfloat) 0 0)
    vertex $ (Vertex3 (0::GLfloat) (-0.2) 0)
    vertex $ (Vertex3 ((-0.2)::GLfloat) (-0.2) 0)
    vertex $ (Vertex3 ((-0.2)::GLfloat) 0 0)
    color $ (Color3 (1::GLfloat) 0 1)
    vertex $ (Vertex3 (0::GLfloat) 0 0)
    vertex $ (Vertex3 (0::GLfloat) 0.2 0)
    vertex $ (Vertex3 ((-0.2::GLfloat)) 0.2 0)
    vertex $ (Vertex3 ((-0.2::GLfloat)) 0 0)

    -- top side
    color  $ (Color3 (1.0::GLfloat) 0 0)
    vertex $ (Vertex3 w w w)
    vertex $ (Vertex3 w w (-w))
    vertex $ (Vertex3 w (-w) (-w))
    vertex $ (Vertex3 w (-w) w)
    vertex $ (Vertex3 w w w)
    vertex $ (Vertex3 w w (-w))
    vertex $ (Vertex3 (-w) w (-w))
    vertex $ (Vertex3 (-w) w w)
    vertex $ (Vertex3 w w w)
    vertex $ (Vertex3 w (-w) w)
    vertex $ (Vertex3 (-w) (-w) w)
    vertex $ (Vertex3 (-w) w w)
    color  $ (Color3 (0.0::GLfloat) 1 0)
    vertex $ (Vertex3 (-w) w w)
    vertex $ (Vertex3 (-w) w (-w))
    vertex $ (Vertex3 (-w) (-w) (-w))
    vertex $ (Vertex3 (-w) (-w) w)
    vertex $ (Vertex3 w (-w) w)
    vertex $ (Vertex3 w (-w) (-w))
    vertex $ (Vertex3 (-w) (-w) (-w))
    vertex $ (Vertex3 (-w) (-w) w)
    vertex $ (Vertex3 w w (-w))
    vertex $ (Vertex3 w (-w) (-w))
    vertex $ (Vertex3 (-w) (-w) (-w))
    vertex $ (Vertex3 (-w) w (-w))
  --}
