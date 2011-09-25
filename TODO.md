# Feature List

* Display Frames-Per-Second on the Screen
* Load Images from File (uncompressed)
* Apply Textures to a cube
* Setup Basic Lighting in Scene [done]
* Refactor World into its own module
  - separate world state from engine state
* Refactor Scene Management into its own module
  - entering scene, exiting scene, etc.
  - layering one scene on top of another (menus?)
  - Actually including a Scene Graph
* Apply more than one texture to a cube (multitexturing)
* Prepare engine for Vertex Shaders
* Prepare engine for Pixel Shaders
* Refactor code so it is idiomatic:
  - Possibly imitate to some degree (http://www.haskell.org/haskellwiki/OpenGLTutorial2)
  - Consider using reactive-glut 
* Consider using Cairo for text/HUD

# Bugs:

* Text is not on orthogonal plane, but rather in 3d scene projection
  - Solution is to use Texture based font system
  - Needs to wait until textures are usable
  - Need to write a [Char] -> Textured Mesh converter
  
