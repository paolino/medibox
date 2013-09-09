module GL where
  
import Graphics.UI.Gtk.OpenGL 
import Graphics.UI.Gtk 
import Graphics.Rendering.OpenGL 

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

mkCanva :: Int -> Int -> (GLWindow -> IO a) -> IO GLDrawingArea
mkCanva  dx dy draw = do
  glconfig <- glConfigNew [GLModeRGBA,
			 GLModeDepth,
			 GLModeDouble]
  canvas <- glDrawingAreaNew glconfig

  widgetSetSizeRequest canvas dx dy
  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  onRealize canvas $ withGLDrawingArea canvas $ \_ -> do
    clearColor $= (Color4 1.0 1.0 1.0 0.0)
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers


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

