
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





