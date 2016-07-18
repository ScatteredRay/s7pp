;; From stuff.scm
(define-macro (while test . body)      ; while loop with predefined break and continue
  `(call-with-exit
    (lambda (break) 
      (let continue ()
	(if (let () ,test)
	    (begin 
	      (let () ,@body)
	      (continue))
	    (break))))))

(define (replace-extension path ext)
    (let ((l 0)
          (end (- (string-length path) 1)))
        (while (and
                 (< l end)
                 (not (eq? (string-ref path (- end l)) #\.)))
            (if (eq? (string-ref path (- end l)) #\/) (begin (set! l -1) (break)))
            (set! l (+ l 1)))
        (string-append (substring path 0 (- end l)) ext)))

(define (obj-file path)
    (string-append "$builddir/" (replace-extension path ".o")))

(define (map-lines fn list)
    (apply append (map (lambda (arg) (append (fn arg) '(#\newline))) list)))
