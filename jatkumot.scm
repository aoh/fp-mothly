
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

(define oma-callcc  ('_sans_cps (λ (k f) (f k (λ (r a) (k a))))))

(oma-callcc
   (lambda (k)
      (print "hello")
      (k "nope")
      (print "world")))

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

(try 
   (print 1)
   (fail "nope") 
   (print 2))

(define (tsekkaa a)
   (if (< a 0) (fail "liian pieni"))
   (if (> a 100) (fail "liian iso"))
   a)

(define (add-safely a b)
   (+ (tsekkaa a) (tsekkaa b)))

(try
   (add-safely 0
      (add-safely 40
        (add-safely 50 -60))))

;;; -------------------------------------------------

(define (print . args)
   (for-each display args)
   (newline))

(define (go-back) #f)
      
(define (joku . args)
   (call-with-current-continuation
      (lambda (k)
         (let ((old-back go-back))
            (set! go-back
               (lambda ()
                  (if (null? args)
                     (begin
                        (set! go-back old-back)
                        (go-back))
                     (let ((a (car args)))
                        (set! args (cdr args))
                        (k a)))))
            (go-back)))))

(let*
   ((a (joku 2 4 6 8 ))
    (b (joku 1 (joku 0.6 1.2 5.6) 5 7 9)))
   (if (< (* a b) 20) 
      (go-back))
   (if (>= (* a b) 30)
      (go-back))
   (print a " * " b " = " (* a b))
   ;(go-back)
   )


;; ---------------------------------------------------------------------
;; scheme


(define (print . args)
   (for-each display args)
   (newline))

(define (tc x) x)
(define threads '())

(define (start!)
   (call-with-current-continuation
      (lambda (exit)
         (set! tc exit)
         (go!))))

(define (go!)
   (if (null? threads)
      (tc 'done)
      (let ((fst (car threads)))
         (set! threads (cdr threads))
         (fst 'resumed))))

(define (next-thread!)
   (call-with-current-continuation
      (lambda (continue)
         (set! threads (append threads (list continue)))
         (go!))))
 
(define-syntax fork
   (syntax-rules ()
      ((fork exp)
         (set! threads (cons (lambda (x) exp (go!)) threads)))))

(define (counter message n)
   (next-thread!)
   (print message n)
   (if (= n 0)
      'done
      (counter message (- n 1))))

(fork (counter "aaaaaaaa at " 8))
(fork (counter "bbbbbb at " 6))
(fork (counter "cccc at " 4))
(fork (counter "dd at " 2))

(start!)

(define-syntax pdefine
   (syntax-rules ()
      ((pdefine (var . args) . body)
         (define (var . args)
            (next-thread!)
            . body))))

(pdefine (laskuri nimi n)
   (if (= n 0)
      (print nimi "bling!")
      (laskuri nimi (- n 1))))

(fork (laskuri "pitkä " 10000))
(fork (laskuri "lyhyt " 10))
(fork (laskuri "keski " 1000))

(start!)
