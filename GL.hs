module GL where
  
import Graphics.UI.Gtk.OpenGL 
import Graphics.UI.Gtk 
import Graphics.Rendering.OpenGL 
import Control.Concurrent.STM
import Control.Monad.Trans

bootGL :: IO ()
bootGL = do 
  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  initGL
  -- initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]  
  depthFunc $= Just Less -- specifies comparison function for DepthBuffer

  clearColor $= Color4 0 0 0 1
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  lineSmooth $= Enabled
  pointSmooth $= Enabled
  polygonSmooth $= Enabled
  shadeModel $= Smooth
  -- We need a OpenGL frame buffer configuration to be able to create other
  -- OpenGL objects.

mkCanva :: TVar (Int,Int) -> (GLWindow -> IO a) -> IO GLDrawingArea
mkCanva size draw = do
  glconfig <- glConfigNew [GLModeRGBA,
			 GLModeDepth,
			 GLModeDouble]
  canvas <- glDrawingAreaNew glconfig

  -- widgetSetSizeRequest canvas dx dy
  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  onRealize canvas $ withGLDrawingArea canvas $ \_ -> do
    -- viewport $= (Position 0 0, Size (fromIntegral dx) (fromIntegral dy))
    clearColor $= (Color4 1.0 1.0 1.0 0.0)
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers

  on canvas configureEvent $ do
	   (w,h) <- eventSize
	   liftIO $ do 
		   atomically $ writeTVar size (w,h)
		   viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
		   matrixMode $= Projection
		   loadIdentity
		   ortho 0.0 1 0.0 1.0 (-1.0) 1.0
		   -- the following line is not in the original example, but it's good style...
		   matrixMode $= Modelview 0
	   return True 
  -- Set the repaint handler
  onExpose canvas $ \_ -> do
    withGLDrawingArea canvas $ \glwindow -> do
      clear [DepthBuffer, ColorBuffer]
      draw glwindow
      glDrawableSwapBuffers glwindow
    return True

  set canvas [widgetCanFocus := True]

  idleAdd (do
    widgetQueueDraw canvas
    return True)
    priorityLow

  return canvas


kcolor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat
kcolor p r g b = Color4 (pn r) (pn g) (pn b) where
	pn n = (p  + n) / (n+1)

mcolor p r g b = color . kcolor p r g b
