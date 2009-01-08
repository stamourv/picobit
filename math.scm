;------------------------------------------------------------------------------

(define-type num
  lo16
  shr16
)

(define min-fix -2)
(define max-fix  2)

(define fixnums (make-vector (+ (- max-fix min-fix) 1)))

(define fixnum
  (lambda (n)
    (vector-ref fixnums (- n min-fix))))

(let loop ((n min-fix))
  (if (<= n max-fix)
      (begin
        (vector-set! fixnums (- n min-fix) (make-num (modulo n #x10000) #f))
        (loop (+ n 1)))))

(let loop ((n min-fix))
  (if (<= n max-fix)
      (begin
        (num-shr16-set!
         (fixnum n)
         (if (< n 0) (fixnum -1) (fixnum 0)))
        (loop (+ n 1)))))

(define minus1 (fixnum -1))
(define zero   (fixnum 0))
(define one    (fixnum 1))

(define make-num
  (let ((mn make-num))
    (lambda (lo16 shr16)
      (if (or (< lo16 0) (> lo16 #xffff))
          (error "lo16 out of range"))
      (mn lo16 shr16))))

(define check-norm
  (lambda (name result)
    (cond ((and (eq? (num-shr16 result) zero)
                (<= (num-lo16 result) max-fix))
           (if (not (eq? result (fixnum (num-lo16 result))))
               (pp (list name "nonneg number is not normalized" (dec result) (num-lo16 result)))))
          ((and (eq? (num-shr16 result) minus1)
                (>= (num-lo16 result) (+ #x10000 min-fix)))
           (if (not (eq? result (fixnum (- (num-lo16 result) #x10000))))
               (pp (list name "neg number is not normalized" (dec result) (- (num-lo16 result) #x10000))))))))

(define check-normalized
  (lambda (name f)
    (lambda args
      (let ((result (apply f args)))
        (check-norm name result)
        result))))

(define invert
  (lambda (x)
    (if (eq? x zero) minus1 zero)))

(define sign?
  (lambda (x)
    (check-norm 'sign?.x x)
    (or (eq? x zero) (eq? x minus1))))

(define neg?
  (lambda (x)
    (check-norm 'neg?.x x)
    (cond ((eq? x zero)   #f)
          ((eq? x minus1) #t)
          (else           (neg? (num-shr16 x))))))

(define lt?
  (lambda (x y)
    (check-norm 'lt?.x x)
    (check-norm 'lt?.y y)
    (< (cmp x y) 0)))

(define le?
  (lambda (x y)
    (check-norm 'le?.x x)
    (check-norm 'le?.y y)
    (<= (cmp x y) 0)))

(define cmp
  (lambda (x y)
    (check-norm 'cmp.x x)
    (check-norm 'cmp.y y)
    (let loop ((x x) (y y) (c 0))
      (cond ((sign? x)
             (cond ((eq? x y) c)
                   ((neg? y)  1)
                   (else      -1)))
            ((sign? y)
             (cond ;((eq? x y) c)
                   ((neg? x)  -1)
                   (else      1)))
            (else
             (loop (num-shr16 x)
                   (num-shr16 y)
                   (let ((x-lo16 (num-lo16 x))
                         (y-lo16 (num-lo16 y)))
                     (cond ((= x-lo16 y-lo16) c)
                           ((< x-lo16 y-lo16) -1)
                           (else              1)))))))))

(define int-len-nonneg
  (lambda (x)
    (check-norm 'int-len-nonneg.x x)
    (let loop1 ((x x) (n 0))
      (let ((shr16 (num-shr16 x)))
        (if (eq? shr16 zero)
            (let loop2 ((lo16 (num-lo16 x)) (n n))
              (if (= lo16 0)
                  n
                  (loop2 (arithmetic-shift lo16 -1)
                         (+ n 1))))
            (loop1 shr16 (+ 16 n)))))))

(define shift-left
  (check-normalized 'shift-left
   (lambda (x n)
     (check-norm 'shift-left.x x)
     (if (< n 0) (error "xxxxxxxxxxx"))
     (if (eq? x zero)
         zero
         (let loop1 ((x x) (n n))
           (if (= (bitwise-and n 15) 0)
               (let loop2 ((x x) (n n))
                 (if (= n 0)
                     x
                     (loop2 (make-num #x0000 x)
                            (- n 16))))
               (loop1 (shl x)
                      (- n 1))))))))

(define shl
  (check-normalized 'shl
   (lambda (x)
     (check-norm 'shl.x x)
     (let loop ((x x) (minus-c zero) (r #f))
       (cond ((eq? x minus-c)
              (norm r x))
             (else
              (let ((z (+ (* 2 (num-lo16 x)) (if (eq? minus-c zero) 0 1))))
                (loop (num-shr16 x)
                      (if (>= z #x10000) minus1 zero)
                      (make-num (modulo z #x10000) r)))))))))

(define shr
  (check-normalized 'shr
   (lambda (x)
     (check-norm 'shr.x x)
     (let loop ((x x) (r #f))
       (cond ((eq? x zero)
              (norm r x))
             ((eq? x minus1)
              (norm r x))
             (else
              (let ((z (+ (arithmetic-shift (num-lo16 x) -1)
                          (if (even? (num-lo16 (num-shr16 x))) #x0000 #x8000))))
                (loop (num-shr16 x)
                      (make-num z r)))))))))

(define add
  (check-normalized 'add
   (lambda (x y)
     (check-norm 'add.x x)
     (check-norm 'add.y y)
     (let loop ((x x) (y y) (minus-c zero) (r #f))
       (cond ((eq? x minus-c)
              (norm r y))
             ((eq? y minus-c)
              (norm r x))
             (else
              (let ((z (+ (num-lo16 x) (num-lo16 y) (if (eq? minus-c zero) 0 1))))
                (loop (num-shr16 x)
                      (num-shr16 y)
                      (if (>= z #x10000) minus1 zero)
                      (make-num (modulo z #x10000) r)))))))))

(define sub
  (check-normalized 'sub
   (lambda (x y)
     (check-norm 'sub.x x)
     (check-norm 'sub.y y)
     (let loop ((x x) (y y) (minus-c minus1) (r #f))
       (cond ((and (eq? x minus-c) (sign? y))
              (norm r (invert y)))
             ((eq? y (invert minus-c))
              (norm r x))
             (else
              (let ((z (+ (num-lo16 x) (- #xffff (num-lo16 y)) (if (eq? minus-c zero) 0 1))))
                (loop (num-shr16 x)
                      (num-shr16 y)
                      (if (>= z #x10000) minus1 zero)
                      (make-num (modulo z #x10000) r)))))))))

(define scale
  (check-normalized 'scale
   (lambda (n16 x)
     (check-norm 'scale.x x)
     (cond ((or (= n16 0)
                (eq? x zero))
            zero)
           ((eq? n16 1)
            x)
           (else
            (let loop ((x x) (carry 0) (r #f))
              (cond ((eq? x zero)
                     (norm r (build-nonneg carry)))
                    ((eq? x minus1)
                     (norm r (build-neg (+ (modulo (- n16) #x10000) carry))))
                    (else
                     (let ((m (+ (* n16 (num-lo16 x)) carry)))
                       (loop (num-shr16 x)
                             (arithmetic-shift m -16)
                             (make-num (modulo m #x10000) r)))))))))))

(define neg
  (check-normalized 'neg
   (lambda (x)
     (check-norm 'neg.x x)
     (sub zero x))))

(define build-nonneg
  (check-normalized 'build-nonneg
   (lambda (lo16)
     (if (<= lo16 max-fix) (fixnum lo16) (make-num lo16 zero)))))

(define build-neg
  (check-normalized 'build-neg
   (lambda (lo16)
     (if (>= lo16 (+ #x10000 min-fix)) (fixnum (- lo16 #x10000)) (make-num lo16 minus1)))))

(define mul
  (check-normalized 'mul
   (lambda (x y)
     (check-norm 'mul.x x)
     (check-norm 'mul.y y)
     (if (neg? x)
         (neg (mulnonneg (neg x) y))
         (mulnonneg x y)))))

(define mulnonneg
  (lambda (x y) ; x is nonnegative
    (check-norm 'mulnonneg.x x)
    (check-norm 'mulnonneg.y y)
    (let loop ((x x) (s zero) (r #f))
      (if (eq? x zero)
          (norm r s)
          (let ((a (add s (scale (num-lo16 x) y))))
            (loop (num-shr16 x)
                  (num-shr16 a)
                  (make-num (num-lo16 a) r)))))))

(define rem
  (check-normalized 'rem
   (lambda (x y)
     (check-norm 'rem.x x)
     (check-norm 'rem.y y)
     (if (neg? x)
         (if (neg? y)
             (neg (cdr (divnonneg (neg x) (neg y))))
             (neg (cdr (divnonneg (neg x) y))))
         (if (neg? y)
             (cdr (divnonneg x (neg y)))
             (cdr (divnonneg x y)))))))

(define div
  (check-normalized 'div
   (lambda (x y)
     (check-norm 'div.x x)
     (check-norm 'div.y y)
     (if (neg? x)
         (if (neg? y)
             (car (divnonneg (neg x) (neg y)))
             (neg (car (divnonneg (neg x) y))))
         (if (neg? y)
             (neg (car (divnonneg x (neg y))))
             (car (divnonneg x y)))))))

(define divnonneg2
  (lambda (x y)
    (let ((x (decode x)) (y (decode y)))
    (let ((lx (integer-length x))
          (ly (integer-length y)))
      (let ((b (- lx ly)))
        (let loop ((i b) (a x) (b (arithmetic-shift y b)) (r 0))
;          (pp (list i a b r))
          (if (< i 0)
              (cons (encode r) (encode a))
              (begin
                (if (<= b a)
                    (loop (- i 1)
                          (- a b)
                          (arithmetic-shift b -1)
                          (+ 1 (* 2 r)))
                    (loop (- i 1)
                          a
                          (arithmetic-shift b -1)
                          (* 2 r)))))))))))

(define divnonneg
  (lambda (x y)
    (check-norm 'divnonneg.x x)
    (check-norm 'divnonneg.y y)
    (let ((lx (int-len-nonneg x))
          (ly (int-len-nonneg y)))
      (if (not (and (= lx (integer-length (decode x)))
                    (= ly (integer-length (decode y)))))
          (error "!!!!"))
      (let ((b (- lx ly)))
        (if (< b 0)
            (cons zero x)
            (let loop ((i b) (a x) (b (shift-left y b)) (r zero))
;              (pp (list i (decode a) (decode b) (decode r)))
              (if (< i 0)
                  (cons r a)
                  (if (le? b a)
                      (loop (- i 1)
                            (sub a b)
                            (shr b)
                            (add one (shl r)))
                      (loop (- i 1)
                            a
                            (shr b)
                            (shl r))))))))))

(define norm2
  (check-normalized 'norm2
   (lambda (shr16 r)
     (if r
         (let ((lo16 (num-lo16 r))
               (newr (num-shr16 r)))
           (norm newr
                 (if (cond ((eq? shr16 zero)   (= lo16 #x0000))
                           ((eq? shr16 minus1) (= lo16 #xffff))
                           (else               #f))
                     shr16
                     (begin
                       (num-shr16-set! r shr16)
                       r))))
         shr16))))

(define norm
  (check-normalized 'norm
   (lambda (r shr16)
     (check-norm 'norm.shr16 shr16)
     (if r
         (let ((lo16 (num-lo16 r))
               (newr (num-shr16 r)))
           (cond ((eq? shr16 zero)
                  (cond ((<= lo16 max-fix)
                         (norm newr (fixnum lo16)))
                        (else
                         (num-shr16-set! r shr16)
                         (norm newr r))))
                 ((eq? shr16 minus1)
                  (cond ((>= lo16 (+ #x10000 min-fix))
                         (norm newr (fixnum (- lo16 #x10000))))
                        (else
                         (num-shr16-set! r shr16)
                         (norm newr r))))
                 (else
                  (num-shr16-set! r shr16)
                  (norm newr r))))
         shr16))))

(define #%list->string
  (lambda (lst)
    (list->string (map integer->char (map decode lst)))))

(define #%cons cons)

(define #%< lt?)
(define #%+ add)
(define #%neg neg)
(define #%quotient div)
(define #%remainder rem)

(define #%number->string
  (lambda (n)
    (#%list->string
     (if (#%< n zero)
         (#%cons (make-num 45 zero) (#%number->string-aux (#%neg n) '()))
         (#%number->string-aux n '())))))

(define #%number->string-aux
  (lambda (n lst)
    (let ((rest (#%cons (#%+ (make-num 48 zero) (#%remainder n (make-num 10 zero))) lst)))
      (if (#%< n (make-num 10 zero))
          rest
          (#%number->string-aux (#%quotient n (make-num 10 zero)) rest)))))

;------------------------------------------------------------------------------

(define fn
 (lambda (i)
   (list (- (expt 2 (+ i -1)) 3)
         (- (expt 2 (+ i -1)) 2)
         (- (expt 2 (+ i -1)) 1)
         (expt 2 (+ i -1))
         (+ (expt 2 (+ i -1)) 1)
         (+ (expt 2 (+ i -1)) 2)
         (+ (expt 2 (+ i -1)) 3)
         
         (- (expt 2 (+ i 0)) 3)
         (- (expt 2 (+ i 0)) 2)
         (- (expt 2 (+ i 0)) 1)
         (expt 2 (+ i 0))
         (+ (expt 2 (+ i 0)) 1)
         (+ (expt 2 (+ i 0)) 2)
         (+ (expt 2 (+ i 0)) 3)
         
         (- (expt 2 (+ i 1)) 3)
         (- (expt 2 (+ i 1)) 2)
         (- (expt 2 (+ i 1)) 1)
         (expt 2 (+ i 1))
         (+ (expt 2 (+ i 1)) 1)
         (+ (expt 2 (+ i 1)) 2)
         (+ (expt 2 (+ i 1)) 3)
         )))

(define nums1
  (append (fn (* 16 1))
          (fn (* 16 2))
          (fn (* 16 3))))

(define nums
  (append (map - (reverse nums1))
          (list -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10)
          nums1))

(define dec
  (lambda (n)
    (cond ((eq? n zero) '(0))
          ((eq? n minus1) '(-1))
          (else (cons (num-lo16 n) (dec (num-shr16 n)))))))

(define decode
  (lambda (n)
    (cond ((eq? n zero) 0)
          ((eq? n minus1) -1)
          (else (+ (* #x10000 (decode (num-shr16 n))) (num-lo16 n))))))

(define encode
  (lambda (n)
    (cond ((and (>= n min-fix)
                (<= n max-fix))
           (fixnum n))
          (else
           (make-num (modulo n #x10000)
                     (encode (arithmetic-shift n -16)))))))

;------------------------------------------------------------------------------

(pp 'cmp)
(for-each
 (lambda (x)
   (for-each
    (lambda (y)
      (let ((a (encode x))
            (b (encode y))
            (c (cond ((< x y) -1) ((= x y) 0) (else 1))))
        (if (not (equal? c (cmp a b)))
            (error "incorrect" x y (dec a) (dec b) c (cmp a b)))))
    nums))
 nums)

(pp 'int-len-nonneg)
(for-each
 (lambda (x)
   (if (>= x 0)
       (let ((a (encode x))
             (c (integer-length x)))
         (if (not (equal? c (int-len-nonneg a)))
             (error "incorrect" x (dec a) c (int-len-nonneg a))))))
 nums)

(pp 'shl)
(for-each
 (lambda (x)
   (let ((a (encode x))
         (c (encode (arithmetic-shift x 1))))
     (if (not (equal? (dec c) (dec (shl a))))
         (error "incorrect" x (dec a) (dec c) (dec (shl a))))))
 nums)

(pp 'shr)
(for-each
 (lambda (x)
   (let ((a (encode x))
         (c (encode (arithmetic-shift x -1))))
     (if (not (equal? (dec c) (dec (shr a))))
         (error "incorrect" x (dec a) (dec c) (dec (shr a))))))
 nums)

(pp 'add)
(for-each
 (lambda (x)
   (for-each
    (lambda (y)
      (let ((a (encode x))
            (b (encode y))
            (c (encode (+ x y))))
        (if (not (equal? (dec c) (dec (add a b))))
            (error "incorrect" x y (dec a) (dec b) (dec c) (dec (add a b))))))
    nums))
 nums)

(pp 'sub)
(for-each
 (lambda (x)
   (for-each
    (lambda (y)
      (let ((a (encode x))
            (b (encode y))
            (c (encode (- x y))))
        (if (not (equal? (dec c) (dec (sub a b))))
            (error "incorrect" x y (dec a) (dec b) (dec c) (dec (sub a b))))))
    nums))
 nums)

(pp 'scale)
(for-each
 (lambda (n16)
   (for-each
    (lambda (x)
      (let ((a (encode x))
            (c (encode (* n16 x))))
        (if (not (equal? (dec c) (dec (scale n16 a))))
            (error "incorrect" x (dec a) (dec c) (dec (scale n16 a))))))
    nums))
 '(0 1 2 3 4 5 6 7 8 9 10 32767 32768 65535))

(pp 'mul)
(for-each
 (lambda (x)
   (for-each
    (lambda (y)
      (let ((a (encode x))
            (b (encode y))
            (c (encode (* x y))))
        (if (not (equal? (dec c) (dec (mul a b))))
            (error "incorrect" x y (dec a) (dec b) (dec c) (dec (mul a b))))))
    nums))
 nums)

(pp 'div)
(for-each
 (lambda (x)
   (for-each
    (lambda (y)
      (if (not (= y 0))
          (let ((a (encode x))
                (b (encode y))
                (c (encode (quotient x y))))
            (if (not (equal? (dec c) (dec (div a b))))
                (error "incorrect" x y (dec a) (dec b) (dec c) (dec (div a b)))))))
    nums))
 nums)

(pp 'rem)
(for-each
 (lambda (x)
   (for-each
    (lambda (y)
      (if (not (= y 0))
          (let ((a (encode x))
                (b (encode y))
                (c (encode (remainder x y))))
            (if (not (equal? (dec c) (dec (rem a b))))
                (error "incorrect" x y (dec a) (dec b) (dec c) (dec (rem a b)))))))
    nums))
 nums)

(pp 'fact)
(define fact
  (lambda (n)
    (if (eq? n zero)
        one
        (mul n (fact (sub n one))))))

(define n (time (fact (encode 50))))
(pp (time (#%number->string n)))
