;; bitwise xor tests

(displayln (= (bitwise-xor #xf0 #x0f) #xff))
(displayln (= (bitwise-xor #x30 #x00) #x30))
(displayln (= (bitwise-xor #x00 #x05) #x05))
(displayln (= (bitwise-xor #x08 #x05) #x0D))
(displayln (= (bitwise-xor #x18 #x05) #x1D))
(displayln (= (bitwise-xor #x18 #x25) #x3D))
(displayln (= (bitwise-xor #x18 #x35) #x2D))
(displayln (= (bitwise-xor #xF7 #xDD) #x2A))
