module Lecture where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

main = toFile def "example1_big.png" $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red]
    plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))
-----------------------------------------------------------------
-----------------------------------------------------------------
reqs  = client initio resps
resps = server reqs

-- client initio (resp:resps) = initio : client (next resp) resps
server      (req:reqs)   = process req : server reqs
client initio ~(resp:resps) = initio : client (next resp) resps

initio        = 0
next resp   = resp
process req = req+1
