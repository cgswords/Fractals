(load "pmatch.scm")

;; z = z - relax * ( z ^ power - 1 )                + pixel
;;                  / ( power * z ^ ( power - 1 ) )
;; 
(define nova-recur
  (lambda (z r p c icount imax)
    (if (>= icount imax) icount
      (let ((nz
              (+ (- z (* r (sub1 (expt z p)) 
                      (/ 1 (* p (expt z (sub1 p)))))) c)))
        (if (or (< (abs (- (cfl-real-part z) (cfl-real-part nz))) 0.001)
                (< (abs (- (cfl-imag-part z) (cfl-imag-part nz))) 0.001))
          icount
          (nova-recur nz r p c (add1 icount) imax))))))

(define nova
  (lambda (r p c imax)
    (trace-lambda nova (y x)
      (cond
        [(and (fixnum? y) (fixnum? x)) 
          (nova-recur 
            (fl-make-rectangular (fixnum->flonum x) (fixnum->flonum y)) 
            r p c 0 imax)]
        [(fixnum? x)     
          (nova-recur 
            (fl-make-rectangular (fixnum->flonum x) y) 
            r p c 0 imax)]
        [(fixnum? y) 
          (nova-recur 
            (fl-make-rectangular x (fixnum->flonum y)) 
            r p c 0 imax)]
        [else 
          (nova-recur 
            (fl-make-rectangular x y) 
            r p c 0 imax)]))))

(define itercombstrf
  (lambda (i j max f outport scale top left)
    (cond
      [(< max i) 
        (begin
          (display "\n" outport)
          (itercombstrf 0 (add1 j) max f outport scale top left))]
      [(< max j) (void)]
      [else 
        (begin
          (display " " outport)
          (display (f (+ top (* i scale)) (+ left (* j scale))) outport)
          (display " " outport)
          (itercombstrf (add1 i) j max f outport scale top left))])))

(define write-pgm-color
  (lambda (file f size ceil top left)
    (if (file-exists? file) (delete-file file))
    (call-with-output-file file
      (lambda (p)
        (display "P3\n" p) 
        (display (number->string size) p)
        (display " " p)
        (display (number->string size) p)
        (display "\n" p)
        ;;(display (number->string (add1 ceil)) p)
        (display "255" p)
        (display "\n" p)
        (itercombstrf 0 0 size f p (/ 1.0 ceil) top left)))))

(define runnovacolor 
  (lambda (file f n m t l)
    (write-pgm-color file f n m t l)))

(define color-lookup
  (lambda (n env)
    (cond
      [(null? env) '(0 0 0)]
      [(and (>= n (caaar env)) (<= n (cdaar env))) (cdar env)]
      [else (color-lookup n (cdr env))])))

(define color-1
  '(
    ((  0 .  5) . (80 80 255))
    ((  6 .  10) . (40 40 20))
    (( 11 .  15) . (0 0 0))
    (( 16 .  25) . (30 30 80))
    (( 26 .  30) . (0 0 200))
    (( 31 .  35) . (80 80 80))
    (( 36 .  40) . (60 40 160))
    (( 41 .  50) . (5 5 5))
    (( 51 .  75) . (0 0 100))
    (( 76 . 100) . (30 30 30))
    ((101 . 125) . (150 150 150))
    ((126 . 150) . (100 0 80))
    ((151 . 175) . (10 0 255))
    ((176 . 200) . (40 40 40))
    ((201 . 225) . (40 40 255))
    ((226 . 255) . (30 30 0))
  ))

(define gen-blue
  (lambda (x)
    `(0 0 ,(min 255 (* x 4)))))

(define gen-darkergray
  (lambda (x)
    (let ((v (max 0 (- 255 (* x 4)))))
      (list v v v))))

(define gen-rainbow
  (lambda (x)
    (let ((v (min 255 (* x x))))
      (cond
        [(< 200 v) (list v (- v x) (- v x))]
        [(< 175 v) (list v v v)]
        [(< 150 v) (list v (- v x) 0)]
        [(< 100 v) (list v 0 0)]
        [(< 50 v) (list v v 0)]
        [(< 25 v) (list v v v)]
        [(< 10 v) (list 0 v 0)]
        [else (list v 0 0)]))))

(define gen-darkgray
  (lambda (x)
    (let ((v (min 255 (* x 4))))
      (list v v v))))

(define color-gen
  (lambda (color-fun color-f)
    (lambda (x y)
      (let* ((v (color-fun x y))
             (colors (color-f v)))
        (pmatch colors
          [(,red ,green ,blue) 
           (string-append 
             (number->string red) " " 
             (number->string green) " " 
             (number->string blue))]))))) 

(define color-set
  (lambda (color-fun colorenv)
    (lambda (x y)
      (let* ((v (color-fun x y))
             (colors (color-lookup v colorenv)))
        (pmatch colors
          [(,red ,green ,blue) 
           (string-append 
             (number->string red) " " 
             (number->string green) " " 
             (number->string blue))])))))
