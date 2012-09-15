{- Textures.hs; 

  Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005 - original author
  Garrett Bluma  (gb@garrettbluma.com)     2012 - modified

This module is for loading textures

-}

module Textures where

import Graphics.Rendering.OpenGL
import PNG (readPng)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (free)

import Debug.Trace

-- read a list of images and returns a list of textures
-- all images are assumed to be in the PNG image format
getAndCreateTextures :: [String] -> IO [Maybe TextureObject]
getAndCreateTextures fileNames = do
   fileNamesExts <- return (map (("png/" ++) . (++ ".png")) fileNames)
   texData <- mapM readImageC fileNamesExts
   texObjs <- mapM createTexture texData
   return texObjs


-- read a single texture
getAndCreateTexture :: String -> IO (Maybe TextureObject)
getAndCreateTexture fileName = do
   texData <- readImageC ("png/" ++ fileName ++ ".png")
   texObj <- createTexture texData
   return texObj


-- read the image data
readImageC :: String -> IO (Maybe (Size, PixelData Word8))
readImageC path 
  | trace ("** [readImageC] Loading image " ++ path) True 
  = catch (readPng path) ( \_ -> do 
      print ("missing texture: "++path)
      return Nothing) 

-- creates the texture
createTexture :: (Maybe (Size, PixelData a)) -> IO (Maybe TextureObject)
createTexture (Just ((Size x y), pixels@(PixelData _ _ ptr))) = do
   [texName] <- genObjectNames 1  -- generate our texture.
   --rowAlignment  Unpack $= 1
   textureBinding Texture2D $= Just texName  -- make our new texture the current texture.
   generateMipmap Texture2D $= Enabled
   build2DMipmaps Texture2D RGBA' (fromIntegral x) (fromIntegral y) pixels
   textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
   textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
   textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
   textureFunction $= Modulate
   -- free ptr -- (TGA needs this, PNG doesn't)
   return (Just texName)
createTexture Nothing = return Nothing

