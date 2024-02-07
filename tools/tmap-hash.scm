;;; hash timings

(define chars-upper "#$%&'()*+,-.0123456789:<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûü")
(define chars-lower "abcdefghijklmnopqrstuvwxyz-?=") ; more schemish? 

(define ok 1000000)
(define bad 10000)
(define horrible 1000)

(define (make-strings chr)
  (let* ((num-keys 10000)
	 (keys (make-vector num-keys))
	 (num-chars (length chr)))
    (do ((i 0 (+ i 1)))
	((= i num-keys)
	 keys)
      (let* ((len (+ 4 (random 12)))
	     (str (make-string len)))
	(do ((j 0 (+ j 1)))
	    ((= j len))
	  (string-set! str j (string-ref chr (random num-chars))))
	(vector-set! keys i str)))))


(define (ref-sym) ; [288, 74 eval]
  (let ((H (make-hash-table 1024))
	(syms (let ((V (make-vector 10000))
		    (strs (make-strings chars-lower)))
		(do ((i 0 (+ i 1)))
		    ((= i 10000) V)
		  (vector-set! V i (string->symbol (vector-ref strs i)))))))
    (do ((i 0 (+ i 1))
	 (sym (vector-ref syms (random 10000)) (vector-ref syms (random 10000))))
	((= i ok))
      (unless (hash-table-ref H sym)
	(hash-table-set! H sym sym)))
    (format *stderr* "ref-sym: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-sym: (6412 9945 27 0 2)

;(ref-sym)


(define (ref-sym1) ; [266, 73 eval]
  (let* ((st (symbol-table))
	 (len (length st)))
    (let ((H (make-hash-table 1024)))
      (do ((i 0 (+ i 1))
	   (sym (vector-ref st (random len)) (vector-ref st (random len))))
	  ((= i ok))
	(unless (hash-table-ref H sym)
	  (hash-table-set! H sym 1)))
      (format *stderr* "ref-sym1: ~A~%" ((object->let H) 'stats:0|1|2|n|max))))) ; ref-sym1: (493 409 111 11 3)

;(ref-sym1)


(define (ref-int) ; [92, 28 in fx_random_i, 17 in hash_int]
  (let ((H (make-hash-table 1024)))
    (do ((i 0 (+ i 1))
	 (int (random 10000) (random 10000)))
	((= i ok))
      (unless (hash-table-ref H int)
	(hash-table-set! H int int)))
    (format *stderr* "ref-int: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-int: (6384 10000 0 0 1)

;(ref-int)


(define (ref-rat) ; [4821, 4546 hash_equal_ratio] this is a worst case -- 0..1 mostly
  (let ((H (make-hash-table 1024)))
    (let ((rats (let ((V (make-vector 10000)))
		  (do ((i 0 (+ i 1)))
		      ((= i 10000) V)
		    (vector-set! V i (/ (+ (random 99) 1) (+ 1 (random 99))))))))
      (do ((i 0 (+ i 1))
	   (rat (vector-ref rats (random 10000)) (vector-ref rats (random 10000))))
	  ((= i bad))
	(unless (hash-table-ref H rat)
	  (hash-table-set! H rat rat)))
      (format *stderr* "ref-rat: ~A~%" ((object->let H) 'stats:0|1|2|n|max))))) ; ref-rat: (16308 36 9 31 2128)

;(ref-rat)


(define (ref-rat1) ; [288, 73 eval]
  (let ((H (make-hash-table 1024)))
    (let ((rats (let ((V (make-vector 10000)))
		  (do ((i 0 (+ i 1)))
		      ((= i 10000) V)
		    (vector-set! V i (/ (+ (random 99999) 1) (+ 1 (random 99))))))))
      (do ((i 0 (+ i 1))
	   (rat (vector-ref rats (random 10000)) (vector-ref rats (random 10000))))
	  ((= i ok))
	(unless (hash-table-ref H rat)
	  (hash-table-set! H rat rat)))
      (format *stderr* "ref-rat1: ~A~%" ((object->let H) 'stats:0|1|2|n|max))))) ; ref-rat1: (12516 1849 637 1382 14)

;(ref-rat1)


(define (ref-float) ; [320, 73 eval, 72 hash_float]
  (let ((H (make-hash-table 1024)))
    (let ((floats (let ((V (make-vector 10000)))
		    (do ((i 0 (+ i 1)))
			((= i 10000) V)
		      (vector-set! V i (random 1000.0))))))
      (do ((i 0 (+ i 1))
	   (float (vector-ref floats (random 10000)) (vector-ref floats (random 10000))))
	  ((= i ok))
	(unless (hash-table-ref H float)
	  (hash-table-set! H float float)))
      (format *stderr* "ref-float: ~A~%" ((object->let H) 'stats:0|1|2|n|max))))) ; ref-float: (15384 2 2 996 21)

;(ref-float)


(define (ref-complex) ; [1133, 945 in hash_float]
  (let ((H (make-hash-table 1024)))
    (let ((cs (let ((V (make-vector 10000)))
		(do ((i 0 (+ i 1)))
		    ((= i 10000) V)
		  (vector-set! V i (complex (random 1000.0) (random 1000.0)))))))
      (do ((i 0 (+ i 1))
	   (c (vector-ref cs (random 10000)) (vector-ref cs (random 10000))))
	  ((= i ok))
	(unless (hash-table-ref H c)
	  (hash-table-set! H c c)))
      (format *stderr* "ref-complex: ~A~%" ((object->let H) 'stats:0|1|2|n|max))))) ; ref-complex: (16374 0 0 10 1065), (14685 199 202 1298 25)

;(ref-complex)

  
(define (ref-string) ; [356 (counting make-strings), 74 eval, 62 for hash_string]
  (let ((H (make-hash-table 1024 string=?))
	(strings (make-strings chars-lower)))
    (do ((i 0 (+ i 1))
	 (str (vector-ref strings (random 10000)) (vector-ref strings (random 10000))))
	((= i ok))
      (unless (hash-table-ref H str)
	(hash-table-set! H str str)))
    (format *stderr* "ref-string: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-string: (12795 1412 686 1491 18)

;(ref-string)


(define (ref-string1) ; [349, 74 eval, 53 hash_string]
  (let ((H (make-hash-table 1024 string=?))
	(strings (make-strings chars-upper)))
    (do ((i 0 (+ i 1))
	 (str (vector-ref strings (random 10000)) (vector-ref strings (random 10000))))
	((= i ok))
      (unless (hash-table-ref H str)
	(hash-table-set! H str str)))
    (format *stderr* "ref-string1: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-string1: (9114 5128 1663 479 6)

;(ref-string1)


(define (ref-string2) ; [3915, 3400 hash_string, 80+73 number_to_string]
  (let ((H (make-hash-table 1024 string=?)))
    (do ((i 0 (+ i 1))
	 (str (string-append "w" (number->string (random 10000))) (string-append "w" (number->string (random 10000)))))
	((= i bad))
      (unless (hash-table-ref H str)
	(hash-table-set! H str str)))
    (format *stderr* "ref-string2: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-string2: (16374 1 0 9 1111)

;(ref-string2)


(define (ref-string3) ; [344, 73 eval]
  (let* ((syms (symbol-table))
	 (len (length syms))
	 (strs (make-vector len)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (vector-set! strs i (symbol->string (vector-ref syms i))))
    (let ((H (make-hash-table 1024)))
      (do ((i 0 (+ i 1))
	   (str (vector-ref strs (random len)) (vector-ref strs (random len))))
	  ((= i ok))
	(unless (hash-table-ref H str)
	  (hash-table-set! H str 1)))
      (format *stderr* "ref-string3: ~A~%" ((object->let H) 'stats:0|1|2|n|max))))) ; ref-string3: (785 112 47 80 26)

;(ref-string3)


(define (ref-ci-string) ; [856, 586 hash_ci_string]
  (let ((H (make-hash-table 1024 string-ci=?))
	(strings (make-strings chars-lower)))
    (do ((i 0 (+ i 1))
	 (str (vector-ref strings (random 10000)) (vector-ref strings (random 10000))))
	((= i ok))
      (unless (hash-table-ref H str)
	(hash-table-set! H str str)))
    (format *stderr* "ref-ci-string: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-ci-string: (16036 0 0 348 42)

;(ref-ci-string)


(define (ref-pair) ; [4574, 2936 in pair_equal, 803 integer_equal, 634 hash_equal_any]
  (let ((H (make-hash-table 1024)))
    (do ((i 0 (+ i 1))
	 (p (cons (random 100) (random 100)) (cons (random 100) (random 100))))
	((= i ok))
      (unless (hash-table-ref H p)
	(hash-table-set! H p p)))
    (format *stderr* "ref-pair: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-pair: (16284 0 0 100 100)

;(ref-pair)


(define (ref-iv) ; [2515, 951 int_vector_equal, 518 iv_meq, 433 hash_equal_any, 305 vector_rank_match]
  (let ((H (make-hash-table 1024)))
    (do ((i 0 (+ i 1))
	 (p (int-vector (random 100) (random 100)) (int-vector (random 100) (random 100))))
	((= i ok))
      (unless (hash-table-ref H p)
	(hash-table-set! H p p)))
    (format *stderr* "ref-iv: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-iv: (16185 2 2 195 100)

;(ref-iv)


(define (ref-bv) ; [2466, 1432 byte_vector_equal, 434 hash_equal_any, 306 vector__rank_match]
  (let ((H (make-hash-table 1024)))
    (do ((i 0 (+ i 1))
	 (p (byte-vector (random 100) (random 100)) (byte-vector (random 100) (random 100))))
	((= i ok))
      (unless (hash-table-ref H p)
	(hash-table-set! H p p)))
    (format *stderr* "ref-bv: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-bv: (16185 2 2 195 100)

;(ref-bv)


(define (ref-v) ; [4038, 2733 vector_equal, 433 hash_equal_any, 305 vector_rank_match, 280 integer_equal]
  (let ((H (make-hash-table 1024)))
    (do ((i 0 (+ i 1))
	 (p (vector (random 100) (random 100)) (vector (random 100) (random 100))))
	((= i ok))
      (unless (hash-table-ref H p)
	(hash-table-set! H p p)))
    (format *stderr* "ref-v: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; (16185 2 2 195 100)

;(ref-v)


(define (ref-fv) ; [2434, 1411 float_vector_equal, 437 hash_equal_any, 300 vector_rank_match]
  (let ((H (make-hash-table 1024)))
    (let ((floats (let ((V (make-vector 10000)))
		    (do ((i 0 (+ i 1)))
			((= i 10000) V)
		      (vector-set! V i (float-vector (random 100.0) (random 100.0)))))))
      (do ((i 0 (+ i 1))
	   (float (vector-ref floats (random 10000)) (vector-ref floats (random 10000))))
	  ((= i ok))
	(unless (hash-table-ref H float)
	  (hash-table-set! H float float)))
      (format *stderr* "ref-fv: ~A~%" ((object->let H) 'stats:0|1|2|n|max))))) ; (16185 1 3 195 118)

;(ref-fv)


(define (ref-let) ; [452, 167 let_equal_1, 65 simple_inlet] -- let_equal checks outlet chains! called from hash_equal_any
  (let ((H (make-hash-table 1024)))
    (do ((i 0 (+ i 1))
	 (p (inlet 'a (random 10000)) (inlet 'a (random 10000)))) ; if two fields, extremely slow?
	((= i ok))
      (unless (hash-table-ref H p)
	(hash-table-set! H p p)))
    (format *stderr* "ref-let: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-let: (6384 10000 0 0 1)

;(ref-let)


(define (ref-let1) ; was extremely slow even if only 10000 calls [10376, 9144 let_equal_1, 586 hash_equal_any, 394 integer_equal, 244 let_equal]
                   ; now [1153, 626 let_equal_1] TODO: use this form below for ref-hash1
  (let ((H (make-hash-table 1024)))
    (let ((lets (let ((V (make-vector 10000)))
		  (do ((i 0 (+ i 1)))
		      ((= i 10000) V)
		    (vector-set! V i (inlet 'a (random 1000) 'b (random 1000)))))))
      (do ((i 0 (+ i 1))
	   (p (vector-ref lets (random 10000)) (vector-ref lets (random 10000))))
	  ((= i ok))
	(unless (hash-table-ref H p)
	  (hash-table-set! H p p)))
      (format *stderr* "ref-let1: ~A~%" ((object->let H) 'stats:0|1|2|n|max))))) ; ref-let1: (14573 208 204 1399 19)

;(ref-let1)


(define (ref-char) ; [114, 26 g_random_i, 12 hash_char, 12 integer_to_char]
  (let ((H (make-hash-table 256 char=?)))
    (do ((i 0 (+ i 1))
	 (c (integer->char (random 256)) (integer->char (random 256))))
	((= i ok))
      (unless (hash-table-ref H c)
	(hash-table-set! H c c)))
    (format *stderr* "ref-char: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; (768 256 0 0 1)

;(ref-char)


;;; 1000 here
(define (ref-hash) ; extremely slow! [8291, 6432 hash_table_equal, 585 hash_equal_any, 536 hash_symbol etc]
  (let ((H (make-hash-table 1024)))
    (do ((i 0 (+ i 1))
	 (p (hash-table 'a (random 10000)) (hash-table 'a (random 10000))))
	((= i horrible))
      (unless (hash-table-ref H p)
	(hash-table-set! H p p)))
    (format *stderr* "ref-hash: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-hash: (16383 0 0 1 6317)

;(ref-hash)


;;; 1000 here
(define (ref-hash1) ; [8258, as above]
  (let ((H (make-hash-table 1024)))
    (do ((i 0 (+ i 1))
	 (p (hash-table 'a (random 100) 'b (random 100)) (hash-table 'a (random 100) 'b (random 100))))
	((= i horrible))
      (unless (hash-table-ref H p)
	(hash-table-set! H p p)))
    (format *stderr* "ref-hash1: ~A~%" ((object->let H) 'stats:0|1|2|n|max)))) ; ref-hash1: (16383 0 0 1 6282)

;(ref-hash1)


(define (all-cases)
  (ref-sym)
  (ref-sym1)
  (ref-int)
  (ref-rat)
  (ref-rat1)
  (ref-float)
  (ref-complex)
  (ref-string)
  (ref-string1)
  (ref-string2)
  (ref-string3)
  (ref-ci-string)
  (ref-pair)
  (ref-iv)
  (ref-bv)
  (ref-fv)
  (ref-v)
  (ref-let)
  (ref-let1)
  (ref-char)
  (ref-hash)
  (ref-hash1)
  )

(all-cases)


#|
;;; (previous limits, not current)
4,686,080,460  s7.c:hash_equal_ratio [/home/bil/motif-snd/repl]
4,545,952,668  s7.c:hash_equal_complex [/home/bil/motif-snd/repl]
3,580,146,875  s7.c:hash_string [/home/bil/motif-snd/repl]
2,935,734,680  s7.c:pair_equal [/home/bil/motif-snd/repl]
2,737,055,600  s7.c:vector_equal [/home/bil/motif-snd/repl]
2,449,985,458  s7.c:hash_equal_any [/home/bil/motif-snd/repl]
1,432,518,786  s7.c:byte_vector_equal [/home/bil/motif-snd/repl]
1,423,077,642  s7.c:float_vector_equal [/home/bil/motif-snd/repl]
1,219,928,760  s7.c:vector_rank_match.constprop.0.isra.0 [/home/bil/motif-snd/repl]
1,113,398,808  s7.c:integer_equal [/home/bil/motif-snd/repl]
  951,717,508  s7.c:int_vector_equal [/home/bil/motif-snd/repl]
|#
