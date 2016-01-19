
(define (fakt x)
   (if (= x 0)
      1
      (* x (fakt (- x 1)))))

(fakt 60)
;;; -----------------------------------------------

(define id (lambda (x) x))

(define (fakt x k)
   (if (= x 0)
      (k 1)
      (fakt (- x 1) 
         (lambda (v) 
            (k (* x v))))))

(fakt 50 id)

;;; -----------------------------------------------

(define cps* (lambda (a b k) (k (* a b))))
(define cps- (lambda (a b k) (k (- a b))))
(define cps= (lambda (a b k) (k (= a b))))

(define cps-fakt 
   (lambda (x k)
      (cps= x 0
         (lambda (res)
            (if (eq? res #t)
               (k 1)
               (cps- x 1
                  (lambda (p)
                     (cps-fakt p
                        (lambda (t)
                           (cps* t x 
                              (lambda (res) 
                                 (k res))))))))))))

(cps-fakt 60 id)

;;; ---------------------------------------------

(call/cc
   (lambda (k)
      (print "hello")
      (k "nope")
      (print "world")))

;;; ----------------------------------------------

(call/cc (lambda (k) (fold (lambda (s a) (if (number? a) (* s a) (k a))) 1 '(1 2 3 4 x))))

;;; ----------------------------------------------

(define (print . args)
   (for-each display args)
   (newline))

(define (fail x)
   (display "bug: no handler"))

(define-syntax try
   (syntax-rules ()
      ((try . exps)
         (call-with-current-continuation
            (lambda (k)
               (let ((old fail))
                  (set! fail
                     (lambda (x)
                        (set! fail old)
                        (k x))))
               . exps)))))

(try (print 1) (fail "nope") (print 2))

;;; -----------------------------------------------

(define (match exp pat)
   (cond
      ((eq? pat '?)
         (list exp))
      ((pair? pat)
         (if (pair? exp)
            (let ((a (match (car exp) (car pat))))
               (cond
                  ((not a)
                     #false)
                  ((null? a)
                     (match (cdr exp) (cdr pat)))
                  (else
                     (append a (match (cdr exp) (cdr pat))))))
            #false))
      ((eq? exp pat)
         null)
      (else #false)))

(define (simple? exp)
   (if (pair? exp)
      (eq? (car exp) 'quote)
      #true))

(define (non-simple? exp)
   (not (simple? exp)))

(define (cps exp)

   (define (cps-any exp cont free)
      (cond
         ((simple? exp)
            (values (list cont exp) free))
         ((match exp '(lambda ? ?))
            (apply
               (lambda (formals body)
                  (lets
                     ((k free)
                      (free (gensym free))
                      (body free (cps-any body k free)))
                     (values
                        (list cont
                           (list 'lambda (cons k formals) body))
                        free)))
               (cdr exp)))
         ((first non-simple? exp #f) =>
            (λ (call)
               (lets
                  ((var free)
                   (free (gensym free))
                   (rest free
                     (cps-any
                        (map (λ (x) (if (eq? x call) var x)) exp)
                        cont free)))
                  (cps-any call `(lambda (,var) ,rest) free))))
         (else
            (values
               (cons (car exp)
                  (cons cont (cdr exp)))
            free))))

   (lets
      ((k (gensym exp))
       (exp free (cps-any exp k (gensym k))))
      exp))

(let loop ()
   (display "cps: ")
   (let ((exp (read stdin)))
      (if (eof? exp)
         'done
         (begin
            (print exp " => " (cps exp))
            (loop)))))

;; ---------------------------------------------------------------------
;; scheme


(define (print . args)
   (for-each display args)
   (newline))

;; (todo . done)
(define threads (cons (list) (list)))
(define tc (lambda (x) x))

(define (go!)
   (if (null? (car threads))
      (begin
         (set-car! threads (cdr threads))
         (set-cdr! threads (list))))
   (if (null? (car threads))
      (tc 'done)
      (let ((next (caar threads)))
         (set-car! threads (cdar threads))
         (next 'resumed))))

(define (next-thread!)
   (call-with-current-continuation
      (lambda (continue)
         (set-cdr! threads (cons continue (cdr threads)))
         (go!))))
 
(define-syntax fork
   (syntax-rules ()
      ((fork exp )
         (set-car! threads (cons (lambda (x) exp (go!)) (car threads))))))

(define (counter message n)
   (next-thread!)
   (print message n)
   (if (= n 0)
      'done
      (counter message (- n 1))))

(define (looper x)
   (next-thread!)
   (looper x))

(fork (counter "a at " 10))
(fork (counter "b at " 20))
(fork (counter "c at " 30))
(fork (counter "d at " 40))

(fork (looper 1))
(fork (looper 2))
(fork (looper 3))
(fork (looper 4))

(define (start!)
   (call-with-current-continuation
      (lambda (exit)
         (set! tc exit)
         (go!))))













