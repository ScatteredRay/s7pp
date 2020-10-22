;;; mock data (method call) timing test

(load "mockery.scm")

(define mock-number (*mock-number* 'mock-number))
(define mock-string (*mock-string* 'mock-string))
(define make-mock-vector (*mock-vector* 'make-mock-vector))
(define make-mock-hash-table (*mock-hash-table* 'make-mock-hash-table))


(define dolph-1
  (let ((+documentation+ "(dolph-1 n gamma) produces a Dolph-Chebyshev FFT data window of 'n' points using 'gamma' as the window parameter."))
    (lambda (N gamma)
      (let ((vals (make-vector N)))
	(let ((alpha (cosh (/ (acosh (expt 10.0 gamma)) N))))
	  (do ((den (/ 1.0 (cosh (* N (acosh alpha)))))
	       (freq (/ pi N))
	       (mult -1 (- mult))
	       (i 0 (+ i 1))
	       (phase (* -0.5 pi)))
	      ((= i N))
	    (set! (vals i) (* mult den (cos (* N (acos (* alpha (cos phase)))))))
	    (set! phase (+ phase freq))))
	;; now take the DFT
	(let ((pk 0.0)
	      (w (make-vector N)))
	  (do ((i 0 (+ i 1))
	       (sum 0.0 0.0))
	      ((= i N))
	    (do ((k 0 (+ k 1)))
		((= k N))
	      (set! sum (+ sum (* (vals k) (exp (/ (* 2.0 0+1.0i pi k i) N))))))
	    (set! (w i) (magnitude sum))
	    (set! pk (max pk (w i))))
	  ;; scale to 1.0 (it's usually pretty close already, that is pk is close to 1.0)
	  (do ((i 0 (+ i 1)))
	      ((= i N))
	    (set! (w i) (/ (w i) pk)))
	  w)))))

(display (dolph-1 (expt 2 8) 0.5)) (newline)
(display (dolph-1 (mock-number (expt 2 8)) (mock-number 0.5))) (newline)


(define src-duration
  (let ((+documentation+ "(src-duration envelope) returns the new duration of a sound after using 'envelope' for time-varying sampling-rate conversion"))
    (lambda (e)
      (let ((len (- (length e) 2)))
	(do ((all-x (- (e len) (e 0))) ; last x - first x
	     (dur 0.0)
	     (i 0 (+ i 2)))
	    ((>= i len) dur)
	  (let ((area (let ((x0 (e i))
			    (x1 (e (+ i 2)))
			    (y0 (e (+ i 1))) ; 1/x x points
			    (y1 (e (+ i 3))))
			(if (< (abs (real-part (- y0 y1))) .0001)
			    (/ (- x1 x0) (* y0 all-x))
			    (/ (* (log (/ y1 y0)) 
				  (- x1 x0)) 
			       (* (- y1 y0) all-x))))))
	    (set! dur (+ dur (abs area)))))))))

(display (src-duration (float-vector 0 1  .1 1  .2 .6  .5 .9  1 .5))) (newline)
(display (src-duration (apply vector (map mock-number '(0 1  .1 1  .2 .6  .5 .9  1 .5))))) (newline)


(define* (cfft data n (dir 1))
  (if (not n) (set! n (length data)))
  (do ((i 0 (+ i 1))
       (j 0))
      ((= i n))
    (if (> j i)
	(let ((temp (data j)))
	  (set! (data j) (data i))
	  (set! (data i) temp)))
    (do ((m (/ n 2)))
	((or (< m 2) (< j m))
	 (set! j (+ j m)))
      (set! j (- j m))
      (set! m (/ m 2))))
  
  (let ((ipow (floor (log n 2)))
	(prev 1))
    (do ((lg 0 (+ lg 1))
	 (mmax 2 (* mmax 2))
	 (pow (/ n 2) (/ pow 2))
	 (theta (complex 0.0 (* pi dir)) (* theta 0.5)))
	((= lg ipow))
      (let ((wpc (exp theta))
	    (wc 1.0))
	(do ((ii 0 (+ ii 1)))
	    ((= ii prev))
	  (do ((jj 0 (+ jj 1))
	       (i ii (+ i mmax))
	       (j (+ ii prev) (+ j mmax)))
	      ((>= jj pow))
	    (let ((tc (* wc (data j))))
	      (set! (data j) (- (data i) tc))
	      (set! (data i) (+ (data i) tc))))
	  (set! wc (* wc wpc)))
	(set! prev mmax))))
  
  data)

(define cfft-size 1024)
(define cfft-data (make-vector cfft-size 0.0))
(do ((i 1 (+ i 1))
     (j (- cfft-size 1) (- j 1)))
    ((= i (/ cfft-size 2)))
  (set! (cfft-data i) (complex (- 1.0 (random 2.0)) (- 1.0 (random 2.0))))
  (set! (cfft-data j) (complex (real-part (cfft-data i)) (- (imag-part (cfft-data i))))))
(define cfft-mdata (copy cfft-data))

(display (cfft cfft-data cfft-size)) (newline)

(let ((mockdata (make-mock-vector cfft-size)))
  (do ((i 0 (+ i 1)))
      ((= i cfft-size))
    (set! (mockdata i) (mock-number (cfft-mdata i))))
  (display (cfft mockdata cfft-size)) (newline))


(define (palindrome? str)
  (or (< (string-length str) 2)
      (and (char=? (string-ref str 0)
		   (string-ref str (- (string-length str) 1)))
	   (palindrome? (substring str 1 (- (string-length str) 1))))))

(display (palindrome? "abcdefgfedcba")) (newline)
(display (palindrome? (mock-string "abcdefgfedcba"))) (newline)


(let ()
  (define (walk p counts)
    (if (pair? p)
	(begin
	  (walk (car p) counts)
	  (if (pair? (cdr p))
	      (walk (cdr p) counts)))
	(hash-table-set! counts p (+ (or (hash-table-ref counts p) 0) 1))))
  
  (define (lint-reader counts)
    (let ((port (open-input-file "lint.scm")))
      (do ((expr (read port) (read port)))
	  ((eof-object? expr)
	   (close-input-port port)
	   counts)
	(walk expr counts))))
  
  (define (sort-counts counts)
    (let ((len (hash-table-entries counts)))
      (do ((v (make-vector len))
	   (h (make-iterator counts))
	   (i 0 (+ i 1)))
	  ((= i len)
	   (sort! v (lambda (e1 e2) (> (cdr e1) (cdr e2))))
	   v)
	(vector-set! v i (iterate h)))))
  
  (display (sort-counts (lint-reader (make-hash-table)))) (newline)
  (display (sort-counts (lint-reader (make-mock-hash-table)))) (newline))


