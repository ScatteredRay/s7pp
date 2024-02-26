;;; multiple-values timing tests

#|
(define (ok? otst ola oexp)
  (let ((result (catch #t ola
		       (lambda (type info)
			 (if (not (eq? oexp 'error))
			     (begin (apply format #t info) (newline)))
			 'error))))
    (if (not (equal? result oexp))
	(format #t "~A: ~A got ~S but expected ~S~%~%" (port-line-number) otst result oexp))))

(define-macro (test tst expected) `(ok? ',tst (#_let () (define (_s7_) ,tst)) ,expected))
|#


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
  (unless (= (mv8) 6) (format *stderr* "mv8: ~S~%" (mv8))) ; op_safe_c_pp_3|6_mv
  (unless (= (mv9) 6) (format *stderr* "mv9: ~S~%" (mv9))) ; op_safe_c_3p_2|3_mv
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

;(mvtest) ; [321] -> [289] -> [281] -> [246]


(define len 1000000)

(define (faddc) ; [607] -> [508 (no pair_append)] -> [384]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (values 1 2 3) 4) 10)
      (display "faddc oops\n" *stderr*))))

;(faddc)


(define (fadds) ; [620] -> [523] -> [396]
  (let ((arg 4))
    (do ((i 0 (+ i 1)))
	((= i len))
      (unless (= (+ (values 1 2 3) arg) 10)
	(display "fadds oops\n" *stderr*)))))

;(fadds)


;(let () (define (func) (do ((x 0.0 (+ x 0.1)) (i 0 (+ i 1))) ((>= x 0.1) (#_with-baffle (inlet (values 1 2) (symbol? x)))))) (func)) */

(define (fadda) ; [626] -> [554] -> [415]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (values 1 2 3) (if (integer? i) 4 0)) 10) ; safe_c_pa_mv
      (display "fadda oops\n" *stderr*))))

;(fadda)


(define (fadd1) ; [834] -> [736 (no pair_append)] -> [718]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (values i (+ i 1) (+ i 2)) 4) (+ 7 (* i 3)))
      (display "fadd1 oops\n" *stderr*))))

;(fadd1)


(define (fadda6) ; [1127 gc copy_proper_list make_list op_safe_c_pa_mv fx_c_opcsq_c] -> [1041] -> [1010]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (values i (+ i 1) 2 3 4 5) (* 2 3)) (+ (* 2 i) 21)) ; op_safe_c_pa_mv > 3 mv vals
      (display "fadda6 oops\n" *stderr*))))

;(fadda6)


(define (fadds6) ; [1010 after] -> [990]
  (let ((three 3))
    (do ((i 0 (+ i 1)))
	((= i len))
      (unless (= (+ (values i (+ i 1) 2 3 4 5) three) (+ (* 2 i) 18))
	(display "fadds6 oops\n" *stderr*)))))

;(fadds6)


(define (faddc6) ; [997 after] -> [978]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (values i (+ i 1) 2 3 4 5) 3) (+ (* 2 i) 18))
      (display "faddc6 oops\n" *stderr*))))

;(faddc6)


(define (fadd2-mv) (values 1 2 3))
(define (fadd2) ; [649] -> [550 (no pair_append)] -> [546 if no goto] -> [425]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (fadd2-mv) 4) 10)
      (display "fadd2 oops\n" *stderr*)))) ; op_c_na calls make_list (op_c_nc?) [op_safe_c_pc_mv? so the make_list can be side-stepped?]

;(fadd2)


(define (faddc0) ; [509] -> [504 plist_4 (lose for extra if, gain in gc)] -> [383]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ 4 (values 1 2 3)) 10) ; safe_c_cp -> safe_c_sp_mv which uses cons(args, value)
      (display "faddc0 oops\n" *stderr*))))

;(faddc0)


(define (fadds02) ; [422 plist_3] -> [409]
  (let ((four 4))
    (do ((i 0 (+ i 1)))
	((= i len))
      (unless (= (+ four (values 1 2)) 7) ; to sp_mv
	(display "fadds02 oops\n" *stderr*)))))

;(fadds02)


(define (fadds0) ; [522] -> [516 plist_4 -- still has make_list] -> [395]
  (let ((four 4))
    (do ((i 0 (+ i 1)))
	((= i len))
      (unless (= (+ four (values 1 2 3)) 10) ; to sp_mv
	(display "fadds0 oops\n" *stderr*)))))

;(fadds0)


(define (fadda0) ; [559] -> [552 plist_4] -> [431]
  (let ((four 2))
    (do ((i 0 (+ i 1)))
	((= i len))
      (unless (= (+ (* 2 four) (values 1 2 3)) 10) ; also goes to sp_mv
	(display "fadda0 oops\n" *stderr*)))))

;(fadda0)


(define (strv) 
  ;; [611 op_safe_c_p -> op_c_p_mv? (copied)] -> [525 (uncopied -- buggy)] -> 
  ;; [679 if safe_list_is_possible (no cancellation)] -> [547 if direct safe_list] ->
  ;; [567 checked safe_list used direct] -> [574 if in_use set] -> [563 if no goto apply]
  ;; [540 if plist] -> [434]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (string=? (string (values #\a #\b #\c)) "abc")
      (display "strv oops\n" *stderr*))))

;(strv)


(define (faddssp2) ; [485] -> [478 if plist] -> [456]
  (let ((four 4))
    (do ((i 0 (+ i 1)))
	((= i len))
      (unless (= (+ four four (values 1 2)) 11)
	(display "faddssp2 oops\n" *stderr*)))))

;(faddssp2)


(define (faddssp3) ; [573] -> [454]
  (let ((four 4))
    (do ((i 0 (+ i 1)))
	((= i len))
      (unless (= (+ four four (values 1 2 3)) 14)
	(display "faddssp3 oops\n" *stderr*)))))

;(faddssp3)


(define (faddp) ; [662] -> [653]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (apply (values + '(1 2))) 3) ; op_c_p_mv
      (display "faddp oops\n" *stderr*))))

;(faddp)


(define (faddap) ; [524] -> [506]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (apply + (values 5 '(1 2))) 8) ; op_c_ap_mv
      (display "faddap oops\n" *stderr*))))

;(faddap)


(define (faddpp) ; [625]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ (values 1 2) (values 3 4)) 10) ; op_safe_c_pp_3|6_mv, also (+ (values 1 2 3) (values 3 -2 -1))
      (display "faddpp oops\n" *stderr*))))

;(faddpp)


(define (fadd3p) ; [784] -> [676 no make_list op_c_nc]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (+ 1 (values 2 3 4) -4) 6) ; op_safe_c_3p_2|3_mv
      (display "fadd3p oops\n" *stderr*))))

;(fadd3p)


(define (faddnp) ; [964]
  (do ((i 0 (+ i 1)))
      ((= i len))
    (unless (= (apply (values + 1 2) '(3)) 6) ; op_any_c_mv (and op_c_na)
      (display "faddnp oops\n" *stderr*))))

;(faddnp)




(define (all-tests)
  (mvtest)
  (faddc)
  (fadds)
  (fadda)
  (fadd1)
  (fadd2)
  (faddc6)
  (fadds6)
  (fadda6)
  (faddc0)
  (fadds02)
  (fadds0)
  (fadda0)
  (strv)
  (faddssp2)
  (faddssp3)
  (faddp)
  (faddap)
  (faddpp)
  (fadd3p)
  (faddnp)
  )

(all-tests)

(when (provided? 'debugging)
  (display ((*s7* 'memory-usage) 'safe-lists))
  (newline))

(exit)
