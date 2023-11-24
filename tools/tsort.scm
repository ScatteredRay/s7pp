;(set! (*s7* 'heap-size) 1024000)
(let ((size 100000))

  (define (less a b)
    (< a b))
  (define (car-less a b)
    (< (car a) (car b)))
  (define (cadr-less a b)
    (< (cadr a) (cadr b)))
  (define (all-less a b)
    (and (< a b)
	 (<= a b)))
  (define (closure-less a b)
    (and (< a b)
	 (= (abs (+ (* 2 (- 3)) 1)) 5))) ; force optimizer to give up!
  (define (begin-less a b)
    (if (and (< a b) (> a b)) (display "oops"))
    (< a b))
  
  (define (check-numbers vc)
    (do ((j 1 (+ j 1)))
	((= j size))
      (if (> (vector-ref vc (- j 1)) (vector-ref vc j))
	  (format *stderr* "~A > ~A?~%" (vector-ref vc (- j 1)) (vector-ref vc j)))))
	 
  (define (check-chars vc)
    (do ((j 1 (+ j 1)))
	((= j size))
      (if (char>? (vector-ref vc (- j 1)) (vector-ref vc j))
	  (format *stderr* "~A > ~A?~%" (vector-ref vc (- j 1)) (vector-ref vc j)))))
    
  (define (check-strings vc)
    (do ((j 1 (+ j 1)))
	((= j size))
      (if (string>? (vector-ref vc (- j 1)) (vector-ref vc j))
	  (format *stderr* "~A > ~A?~%" (vector-ref vc (- j 1)) (vector-ref vc j)))))

  (define (check-car vc)
    (do ((j 1 (+ j 1)))
	((= j size))
      (if (> (car (vector-ref vc (- j 1))) (car (vector-ref vc j)))
	  (format *stderr* "~A > ~A?~%" (car (vector-ref vc (- j 1))) (car (vector-ref vc j))))))
  
  (define (check-cdr vc)
    (do ((j 1 (+ j 1)))
	((= j size))
      (if (> (cdr (vector-ref vc (- j 1))) (cdr (vector-ref vc j)))
	  (format *stderr* "~A > ~A?~%" (cdr (vector-ref vc (- j 1))) (cdr (vector-ref vc j))))))
  
  (define (check-cadr vc)
    (do ((j 1 (+ j 1)))
	((= j size))
      (if (> (cadr (vector-ref vc (- j 1))) (cadr (vector-ref vc j)))
	  (format *stderr* "~A > ~A?~%" (cadr (vector-ref vc (- j 1))) (cadr (vector-ref vc j))))))
	 
  (define (vfill-n v x)
    (do ((i 0 (+ i 1)))
	((= i size))
      (vector-set! v i (random x))))
  
  (define (vfill-s v)
    (do ((i 0 (+ i 1)))
	((= i size))
      (vector-set! v i (string (integer->char (random 256)) (integer->char (random 256))))))

  (define (vfill-c v)
    (do ((i 0 (+ i 1)))
	((= i size))
      (vector-set! v i (integer->char (random 256)))))
  
  (define (vfill-p v)
    (do ((i 0 (+ i 1)))
	((= i size))
      (vector-set! v i (cons (random 1.0) (random 100000)))))

  (define (vfill-r v)
    (do ((i 0 (+ i 1)))
	((= i size))
      (vector-set! v i (list 0.0 (random 100000)))))

  (define (vfill-v v)
    (do ((i 0 (+ i 1)))
	((= i size))
      (vector-set! v i (symbol "a" (number->string i)))))

  (define (run-sort)
    (let ((v (make-vector size)))
      (vfill-n v 1.0)
      (let ((vc (copy v)))
	(sort! vc <)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc less)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc all-less)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc begin-less)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc closure-less)
	(check-numbers vc))
      (sort! v (lambda (a b) (< a b)))
      (check-numbers v))
    
    (let ((v (make-float-vector size)))
      (vfill-n v 100.0)
      (let ((vc (copy v)))
	(sort! vc <)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc less)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc all-less)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc begin-less)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc closure-less)
	(check-numbers vc))
      (sort! v (lambda (a b) (< a b)))
      (check-numbers v))
    
    (let ((v (make-int-vector size)))
      (vfill-n v 10000000)
      (let ((vc (copy v)))
	(sort! vc <)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc less)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc all-less)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc begin-less)
	(check-numbers vc))
      (let ((vc (copy v)))
	(sort! vc closure-less)
	(check-numbers vc))
      (sort! v (lambda (a b) (< a b)))
      (check-numbers v))
    
    (let ((v (make-vector size)))
      (vfill-s v)
      (let ((vc (copy v)))
	(sort! vc string<?)
	(check-strings vc))
      (sort! v (lambda (a b) (string<? a b)))
      (check-strings v)
      
      (vfill-c v)
      (let ((vc (copy v)))
	(sort! vc char<?)
	(check-chars vc))
      (sort! v (lambda (a b) (char<? a b)))
      (check-chars v)
      
      (vfill-p v)
      (let ((vc (copy v)))
	(sort! vc (lambda (a b) (< (car a) (car b))))
	(check-car vc))
      (let ((vc (copy v)))
	(sort! vc car-less)
	(check-car vc))
      (sort! v (lambda (a b) (< (cdr a) (cdr b))))
      (check-cdr v)
      
      (vfill-r v)
      (let ((vc (copy v)))
	(sort! vc (lambda (a b) (< (cadr a) (cadr b))))
	(check-cadr vc))
      (let ((vc (copy v)))
	(sort! vc cadr-less)
	(check-cadr vc))
      
      (let ((v (make-vector size)))
	(vfill-v v)
	(sort! v (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
      ))
  
  (run-sort))

(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)
