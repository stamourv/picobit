;; bitwise or tests

(displayln (= (bitwise-ior #xf0 #x0f) #xff))
(displayln (= (bitwise-ior #x30 #x00) #x30))
(displayln (= (bitwise-ior #x00 #x05) #x05))
(displayln (= (bitwise-ior #x08 #x05) #x0D))
(displayln (= (bitwise-ior #x18 #x05) #x1D))
(displayln (= (bitwise-ior #x18 #x25) #x3D))
(displayln (= (bitwise-ior #x18 #x35) #x3D))
(displayln (= (bitwise-ior #x1823122312 #x351234123456) 58386709362518))
