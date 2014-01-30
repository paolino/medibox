
module SupercolliderComm where

import Sound.OSC
import Sound.SC3 hiding (pitch)


withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110]

			
			
			
				
		

