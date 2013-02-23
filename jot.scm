(load "pmatch.scm")

(define k '(lambda (x) (lambda (y) x)))
(define s '(lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))

(set! vc 0)
(set! maxval 500)

(define update
  (lambda ()
    (if (< vc maxval)
        (begin (set! vc (add1 vc)) #t)
        #f)))

(define-syntax lambdac
  (syntax-rules ()
    [(lambdac (x) body) 
      (lambda (x)
        (if (update) body (lambda (x) x)))]))

(define jot 
  (lambda (exp) 
    (let jot ((v (lambdac (x) x)) (exp exp))
      (cond
        [(null? exp) v]
        [(eq? (car exp) 1) (jot (lambdac (f) (lambdac (a) (v (f a )))) (cdr exp))]
        [else (jot ((v (lambdac (x) (lambdac (y) (lambdac (z) ((x z) (y z))))))
                  (lambdac (x) (lambdac (y) x)))
                 (cdr exp))]))))

(define val
  (lambda (exp)
    (let ((ret vc))
      (set! vc 0)
      ret)))

(define itercomb
  (lambda (i j max f outport)
    (cond
      [(< max i) 
        (begin
          (display "\n" outport)
          (itercomb 0 (add1 j) max f outport))]
      [(< max j) (void)]
      [else 
        (begin
          (display " " outport)
          (display (number->string (f i j)) outport)
          (display " " outport)
          (itercomb (add1 i) j max f outport))])))

(define itercombstrf
  (lambda (i j max f outport)
    (cond
      [(< max i) 
        (begin
          (display "\n" outport)
          (itercombstrf 0 (add1 j) max f outport))]
      [(< max j) (void)]
      [else 
        (begin
          (display " " outport)
          (display (f i j) outport)
          (display " " outport)
          (itercombstrf (add1 i) j max f outport))])))

(define jotcnt
  (lambda (f) 
    (lambda (x y) 
      (val (jot (f x y))))))

(define write-pgm
  (lambda (file f size ceil)
    (set! maxval ceil)
    (if (file-exists? file) (delete-file file))
    (call-with-output-file file
      (lambda (p)
        (display "P2\n" p) 
        (display (number->string size) p)
        (display " " p)
        (display (number->string size) p)
        (display "\n" p)
        (display (number->string ceil) p)
        (display "\n" p)
        (itercomb 0 0 size (jotcnt f) p)))))

(define write-pgm-color
  (lambda (file f size ceil)
    (set! maxval ceil)
    (if (file-exists? file) (delete-file file))
    (call-with-output-file file
      (lambda (p)
        (display "P3\n" p) 
        (display (number->string size) p)
        (display " " p)
        (display (number->string size) p)
        (display "\n" p)
        (display (number->string ceil) p)
        (display "\n" p)
        (itercombstrf 0 0 size f p)))))

(define runjot 
  (lambda (file f n m)
    (write-pgm file f n m)))

(define runjotcolor 
  (lambda (file f n m)
    (if (> m 255) (set! m 200))
    (write-pgm-color file f n m)))

(define dec2bin
  (lambda (n)
    (cond
      [(< n 2) (list n)]
      [(zero? (mod n 2)) (cons 0 (dec2bin (/ n 2)))]
      [else (cons 1 (dec2bin (/ (sub1 n) 2)))])))

(define binstrapp
  (lambda (x y)
    (append (reverse (dec2bin x)) (reverse (dec2bin y)))))

(define numxor
  (lambda (x y)
    (if (= x y) 0 1)))

(define numor
  (lambda (x y) (if (or (= 1 x) (= 1 y)) 1 0)))

(define numand
  (lambda (x y) (if (or (zero? x) (zero? y)) 0 1)))

(define map-with-0s
  (lambda (f l1 l2)
    (cond
      [(and (null? l1) (null? l2)) '()]
      [(null? l1) (cons (f 0 (car l2)) (map-with-0s f l1 (cdr l2)))]
      [(null? l2) (cons (f (car l1) 0) (map-with-0s f (cdr l1) l2))]
      [else (cons (f (car l1) (car l2)) (map-with-0s f (cdr l1) (cdr l2)))])))

(define map-with-1s
  (lambda (f l1 l2)
    (cond
      [(and (null? l1) (null? l2)) '()]
      [(null? l1) (cons (f 1 (car l2)) (map-with-1s f l1 (cdr l2)))]
      [(null? l2) (cons (f (car l1) 1) (map-with-1s f (cdr l1) l2))]
      [else (cons (f (car l1) (car l2)) (map-with-1s f (cdr l1) (cdr l2)))])))

(define getnof
  (lambda (n a)
    (define nfaster
      (lambda (n ls)
        (cond
          [(zero? n) ls]
          [else (nfaster (sub1 n) (cons a ls))])))
    (nfaster n '())))

(define binstrxorz
  (lambda (x y)
    (map-with-0s numxor (reverse (dec2bin x)) (reverse (dec2bin y)))))

(define binstrxoro
  (lambda (x y)
    (map-with-1s numxor (reverse (dec2bin x)) (reverse (dec2bin y)))))

(define binstrorz
  (lambda (x y)
    (map-with-0s numor (reverse (dec2bin x)) (reverse (dec2bin y)))))

(define binstroro
  (lambda (x y)
    (map-with-1s numor (reverse (dec2bin x)) (reverse (dec2bin y)))))

(define binstrandz
  (lambda (x y)
    (map-with-0s numand (reverse (dec2bin x)) (reverse (dec2bin y)))))

(define binstrando
  (lambda (x y)
    (map-with-1s numand (reverse (dec2bin x)) (reverse (dec2bin y)))))

(define bitshiftz
  (lambda (x y)
    (append (getnof y 0) (reverse (dec2bin x)))))

(define bitshifto
  (lambda (x y)
    (append (getnof y 1) (reverse (dec2bin x)))))

(define rbcolors
  (lambda (x y)
    (let ((l1val (val (jot (reverse (dec2bin x)))))
          (l2val (val (jot (reverse (dec2bin y))))))
      (string-append (number->string l1val) " " (number->string 0) " " (number->string l2val)))))

(define rbcolors2
  (lambda (x y)
    (let ((l1val (val (jot (reverse (dec2bin x)))))
          (l2val (val (jot (reverse (dec2bin y))))))
      (let ((red   (min 255 (+ l1val l2val)))
            (green (min 255 (abs (- l1val l2val))))
            (blue  (min 255 (mod l1val (add1 l2val)))))
        (string-append 
          (number->string red) " " 
          (number->string green) " " 
          (number->string blue))))))

(define rbcolors3
  (lambda (x y)
    (let ((l1val (val (jot (binstrando x y))))
          (l2val (val (jot (binstrandz x y)))))
      (let ((red   (min 255 (+ l1val l2val)))
            (green (min 255 (abs (- l1val l2val))))
            (blue  (min 255 (mod l1val (add1 l2val)))))
        (string-append 
          (number->string red) " " 
          (number->string green) " " 
          (number->string blue))))))

