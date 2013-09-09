---------------------------------------------
------------------------------------------
data Polyg = Polyg Int 



instance Sprite Polyg where
	connections (Polyg n) =  [(0.5 + 0.92 * cos (p i)/2 , 0.5 + 0.92 * sin (p i)/2) | i <- [0 .. n - 1]]where
		p i = fromIntegral i * 2 * pi / fromIntegral n

	renderSprite (Polyg n) = 
			renderPrimitive LineLoop $ do
				forM_ [0..n - 1] $ \i -> do
					let step = fromIntegral i * 2 * pi / fromIntegral n
					vertex (Vertex2 (0.5 + cos step/2) (0.5 +  sin step/2) :: Vertex2 GLfloat)
-- $(makeLenses ''Graph)


somePoly = map (MSprite (0.5,0.5) (0.1,0.1)) 
	[	Polyg 3
	,	Polyg 4
	,	Polyg 4
	,	Polyg 5
	,	Polyg 5
	,	Polyg 3	
	]
data CSynth = CSynth 
        {       _csi :: Int
        ,       _csname :: String
        ,       _csparams :: [String]
        }
someSynths = map (MSprite (0.5,0.5) (0.1,0.1)) 
        [       CSynth 0 "PIPPO" ["AMPL","FEEDB","LATCH","RETA"] 
        ,       CSynth 0 "PIPPO" ["AMPL","FEEDB","LATCH","RETA"] 
        ,       CSynth 0 "PIPPO" ["AMPL","FEEDB","LATCH","RETA"] 
        ,       CSynth 0 "PIPPO" ["AMPL","FEEDB","LATCH","RETA"] 
        ,       CSynth 0 "PIPPO" ["AMPL","FEEDB","LATCH","RETA"] 
        ]
instance Sprite CSynth where
        connections (CSynth i n ps) =  
		(zip (repeat 0.92) $ 1.5 * h : [1.5 * h + fromIntegral i * h | i <-  [0 .. length ps - 1]])  ++
		(zip (repeat 0.08) $ 1.5 * h : [1.5 * h + fromIntegral i * h | i <-  [0 .. length ps - 1]]) where
		h = 1/fromIntegral (length ps  + 1)

        renderSprite (CSynth i n ps) = do
			let	h = 1/(fromIntegral $ length ps + 1) 
                        renderPrimitive LineLoop $ do
                                vertex (Vertex2 0 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 1 :: Vertex2 GLfloat)
			forM_ (zip [0,h..] (n:ps)) $ \(d,n) -> 
				renderWordPOSWH 0.5 0.5 (1/10) (d + h/4) (1/15) (h/2) $ take 9 n

  ref <- newTVarIO  $ Graph 
	(IM.fromList . zip [0..] $ zipWith CSprite (repeat 0) someSynths ++  zipWith CSprite (repeat 0) somePoly)
        IM.empty
