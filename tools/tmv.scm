
(define (ok? otst ola oexp)
  (let ((result (catch #t ola
		       (lambda (type info)
			 (if (not (eq? oexp 'error))
			     (begin (apply format #t info) (newline)))
			 'error))))
    (if (not (equal? result oexp))
	(format #t "~A: ~A got ~S but expected ~S~%~%" (port-line-number) otst result oexp))))

(define-macro (test tst expected) `(ok? ',tst (#_let () (define (_s7_) ,tst)) ,expected))


;;; -------- multiple values from tmisc --------
(define (mv1)
  (+ (values 1 2 3)))
(define (mv2)
  (+ 1 (values 2 3)))
(define (mv3)
  (+ (values 1 2) 3))
(define (mv4 x)
  (+ x (values x x)))
(define (mv5 x)
  (+ (values x x) x))
(define (mv-clo1 x y)
  (+ x y))
(define (mv6 x)
  (mv-clo1 (values x 1)))
(define (mv-clo2 . args)
  (apply + args))
(define (mv7 x)
  (mv-clo2 (values x 1)))
(define (mv8)
  (+ (values 1 2 3) (values 3 -2 -1)))
(define (mv9)
  (+ 1 (values 2 3 4) -4))
(define (mv11)
  (+ 1 (values -1 2 4)))
(define (mv12 x y)
  (+ x y (values 2 3 4)))

;;; pair_sym: (mv-clo (values x 1)), h_c_aa: (values x 1), splice_eval_args2 ([i] 1), eval_arg2->apply mv-clo! (loop below is safe_dotimes_step_p
;;;   not enough args for mv-clo1? 
;;; mv-clo2: closure_s_p -> pair_sym ->h_c_aa etc as above!
;;;   perhaps apply_[safe_]closure?

(define (mvtest)
  (unless (= (mv1) 6) (format *stderr* "mv1: ~S~%" (mv1)))
  (unless (= (mv2) 6) (format *stderr* "mv2: ~S~%" (mv2)))
  (unless (= (mv3) 6) (format *stderr* "mv3: ~S~%" (mv3)))
  (unless (= (mv4 2) 6) (format *stderr* "(mv4 2): ~S~%" (mv4 2)))
  (unless (= (mv5 2) 6) (format *stderr* "(mv5 2): ~S~%" (mv5 2)))
  (unless (= (mv6 5) 6) (format *stderr* "(mv6 5): ~S~%" (mv6 5)))
  (unless (= (mv7 5) 6) (format *stderr* "(mv7 5): ~S~%" (mv7 5)))
  (unless (= (mv8) 6) (format *stderr* "mv8: ~S~%" (mv8)))
  (unless (= (mv9) 6) (format *stderr* "mv9: ~S~%" (mv9)))
  (unless (= (mv11) 6) (format *stderr* "mv11: ~S~%" (mv11)))
  (unless (= (mv12 -1 -2) 6) (format *stderr* "(mv12 -1 -2): ~S~%" (mv12 -1 -2)))
  (do ((i 0 (+ i 1)))
      ((= i 50000))
    (mv1)
    (mv2)
    (mv3)
    (mv4 i)
    (mv5 i)
    (mv6 i)
    (mv7 i)
    (mv8)
    (mv9)
    (mv11)
    (mv12 -2 -1)
    ))

;(mvtest) ; [321] -> [289]


(define len 1000000)

(define (fadd) ; [607] -> [508 (no pair_append)]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (values 1 2 3) 4) 10)
      (display "fadd oops\n" *stderr*))))

;(fadd)


(define (fadds) ; [620] -> [523]
  (let ((arg 4))
    (do ((i 0 (+ i 1)))
	((= i len))
      (unless (= (+ (values 1 2 3) arg) 10)
	(display "fadds oops\n" *stderr*)))))

;(fadds)


;(let () (define (func) (do ((x 0.0 (+ x 0.1)) (i 0 (+ i 1))) ((>= x 0.1) (#_with-baffle (inlet (values 1 2) (symbol? x)))))) (func)) */

(define (fadda) ; [626] -> [554]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (values 1 2 3) (if (integer? i) 4 0)) 10) ; safe_c_pa_mv
      (display "fadda oops\n" *stderr*))))

;(fadda)


(define (fadd0) ; 509
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ 4 (values 1 2 3)) 10) ; safe_c_cp -> safe_c_sp_mv which uses cons(args, value)
      (display "fadd0 oops\n" *stderr*))))

;(fadd0)


(define (fadd1) ; [834] -> [736 (no pair_append)]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (values i (+ i 1) (+ i 2)) 4) (+ 7 (* i 3)))
      (display "fadd1 oops\n" *stderr*))))

;(fadd1)


(define (fadd2-mv) (values 1 2 3))
(define (fadd2) ; [649] -> [550 (no pair_append)]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (fadd2-mv) 4) 10)
      (display "fadd2 oops\n" *stderr*)))) ; op_c_na calls make_list (op_c_nc?) [op_safe_c_pc_mv? so the make_list can be side-stepped?]

(fadd2)




(define (all-tests)
  (mvtest)
  (fadd)
  (fadds)
  (fadda)
  (fadd0)
  (fadd1)
  (fadd2)
  )

;(all-tests)
