(define s '("élan ‘quote’")) ; example from Norman Gray
(display s)       ; ("élan â\x80;\x98;quoteâ\x80;\x99;")
(newline)
(display (car s)) ; élan ‘quote’
(newline)

(define b (string->byte-vector (car s)))
(format #t "~{~X ~}" b) ;c3 a9 6c 61 6e 20 e2 80 98 71 75 6f 74 65 e2 80 99 
(newline)

(load "libutf8proc.scm")

(with-let *libutf8proc*
  (define p (utf8proc_map (car s) UTF8PROC_NULLTERM)) ; is this doing anything useful (besides error checking)?
  (display (car p))       ; élan ‘quote’
  (newline)

  (define p1 (utf8proc_map "(\"élan ‘quote’\")" UTF8PROC_NULLTERM))
  (display (car p1))      ; ("élan ‘quote’")
  (newline)

  (define b1 (string->byte-vector (car p1)))
  (format #t "~{~X ~}" b1) ;28 22 c3 a9 6c 61 6e 20 e2 80 98 71 75 6f 74 65 e2 80 99 22 29
  (newline)

  ;; this will produce an inconsistent UTF-8 string
  (define s1 (with-output-to-string (lambda () (display s))))
  (define p2 (utf8proc_map s1 UTF8PROC_NULLTERM))
  (if (integer? (cdr p2))
      (display (utf8proc_errmsg (cdr p2))) ; "Invalid UTF-8 string"
      (display (car p2)))
  (newline)

  (let ((len (cdr p1))
	(p1c (copy (car p1))))
    (do ((n (utf8proc_iterate p1c len) (utf8proc_iterate p1c len)))
	((<= (car n) 0)) ; (cdr n) is the codepoint as an integer
      (display (substring p1c 0 (car n))) (display #\space)
      (set! p1c (substring p1c (car n)))
      (set! len (- len (car n))))
    (newline))

  (let ((e1 (utf8proc_encode_char #x00E9))) ; unicode code-point to utf-8 -> (cons utf-8-string length-thereof)
    (format #t "~{~X~}" (string->byte-vector (car e1)))
    (newline)
    (display (car e1))
    (newline))
  
  )
