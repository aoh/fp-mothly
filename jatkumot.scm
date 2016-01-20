
;; Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary.

; monadit kategoriateoria kontinuaatiot tyyppiteoria 

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

(define-syntax CPS
   (syntax-rules (if quote)
      ((CPS k (lambda (arg ...) body))
         (k (lambda (x arg ...) (CPS x body))))
      ((CPS k (x ... (op . args) y ...))
         (CPS
            (lambda (r)
               (CPS k (x ... r y ...)))
            (op . args)))
      ((CPS k (quote val))
         (k (quote val)))
      ((CPS k (if (op . args) a b))
         (CPS 
            (lambda (test) (CPS k (if test a b)))
            (op . args)))
      ((CPS k (if test a b))
         (if test
            (CPS k a)
            (CPS k b)))
      ((CPS k (rator rand ...))
         (rator k rand ...))
      ((CPS k x)
         (k x))
      ((CPS value)
         (lambda (k) (CPS k value)))))

,expand (CPS 42)
,expand (CPS (+ 1 2))
,expand (CPS (+ 1 (+ 2 3)))
,expand (CPS (lambda (x) (x x)))

(define (add-cps k a b) (k (+ a b)))

((CPS (add-cps 111 222)) (lambda (x) x))

;;; ---------------------------------------------

(call/cc
   (lambda (k)
      (print "hello")
      (k "goodbye")
      (print "world")))

;;; ----------------------------------------------

(fold (lambda (s a) (* s a)) 1 '(1 2 3 4 5))

(fold (lambda (s a) (* s a)) 1 '(1 2 3 4 5 x))

(call/cc (lambda (ret) (fold (lambda (s a) (* s a)) 1 '(1 2 3 4 5))))

(call/cc (lambda (ret) (fold (lambda (s a) (if (not (number? a)) (ret a)) (* s a)) 1 '(1 2 3 4 5))))

;;; ----------------------------------------------

(define oma-callcc  ('_sans_cps (λ (k f) (f k (λ (r a) (k a))))))

(oma-callcc
   (lambda (k)
      (print "hello")
      (k "BYE")
      (print "world")))

;;; ----------------------------------------------

(define foo 42)
(define k (call/cc (lambda (x) x)))
k
(define bar 100)
(k 'hello)
k bar?

;;; ----------------------------------------------
;;; SWITCH TO SCHEME

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
   (print "" 
      (add-safely 0
         (add-safely 40
           (add-safely 50 -60)))))

;;; -------------------------------------------------

(define (print . args)
   (for-each display args)
   (newline))

(define (ei) #f)
      
(define (joku . args)
   (call-with-current-continuation
      (lambda (k)
         (let ((old-back ei))
            (set! ei
               (lambda ()
                  (if (null? args)
                     (begin
                        (set! ei old-back)
                        (ei))
                     (let ((a (car args)))
                        (set! args (cdr args))
                        (k a)))))
            (ei)))))

(let*
   ((a (joku 2 4 6 8 ))
    (b (joku 1 (joku 0.6 1.2 5.6) 5 7 9)))
   (if (< (* a b) 20) 
      (ei))
   (if (>= (* a b) 30)
      (ei))
   (print a " * " b " = " (* a b))
   (ei))


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

(fork (counter "aaaa at " 4))
(fork (counter "bbb at " 3))
(fork (counter "cc at " 2))
(fork (counter "d at " 1))

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

(fork (laskuri "pitkä " 100000))
(fork (laskuri "lyhyt " 10))
(fork (laskuri "keski " 100))

(start!)

;; räntit

; try/catch
; generaattorit
; threadit
; callbackit


