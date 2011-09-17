module Cube where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-------------------------------------------------------------
cube w = do
  renderPrimitive Quads $ do

    -- draw right face
    color $ (Color3 (1.0::GLfloat) 0 0)
    vertex $ (Vertex3   w    w    w  )
    vertex $ (Vertex3   w    w  (-w) )
    vertex $ (Vertex3   w  (-w) (-w) )
    vertex $ (Vertex3   w  (-w)   w  )

    -- draw back face
    color $ (Color3 (0::GLfloat) 0 1)
    vertex $ (Vertex3   w    w  (-w) )
    vertex $ (Vertex3 (-w)   w  (-w) )
    vertex $ (Vertex3 (-w) (-w) (-w) )
    vertex $ (Vertex3   w  (-w) (-w) )

    -- draw left face
    color $ (Color3 (1::GLfloat) 0 1)
    vertex $ (Vertex3 (-w)   w  (-w) )
    vertex $ (Vertex3 (-w)   w    w  )
    vertex $ (Vertex3 (-w) (-w)   w  )
    vertex $ (Vertex3 (-w) (-w) (-w) )

    -- draw front face
    color $ (Color3 (0.3::GLfloat) 0.2 0.1)
    vertex $ (Vertex3 (-w)   w    w  )
    vertex $ (Vertex3   w    w    w  )
    vertex $ (Vertex3   w  (-w)   w  )
    vertex $ (Vertex3 (-w) (-w)   w  )

    -- draw top face
    color $ (Color3 (0::GLfloat) 1 1)
    vertex $ (Vertex3   w    w    w  )
    vertex $ (Vertex3 (-w)   w    w  )
    vertex $ (Vertex3 (-w)   w  (-w) )
    vertex $ (Vertex3   w    w  (-w) )

    -- draw bottom face
    color $ (Color3 (1::GLfloat) 1 0.5)
    vertex $ (Vertex3 (-w) (-w)   w  )
    vertex $ (Vertex3 (-w) (-w) (-w) )
    vertex $ (Vertex3   w  (-w) (-w) )
    vertex $ (Vertex3   w  (-w)   w  )
