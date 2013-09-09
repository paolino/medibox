
main = do
  bootGL
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8,
                   windowTitle := "tracks widget" ]
  hb <- hBoxNew False 1
  connects <- graphing 
  boxPackStart hb connects PackNatural 0 

  frame <- frameNew
  set frame [containerChild := hb]
  gbox <- vBoxNew False 1
  l <- labelNew $ Just "Medibox (0.1)"

  boxPackStart gbox l PackGrow  0
  boxPackStart gbox frame PackNatural 0
  set window [containerChild := gbox] 
  widgetShowAll window
  dat <- widgetGetDrawWindow $ window
  cursorNew Tcross >>= drawWindowSetCursor dat . Just 
  mainGUI
