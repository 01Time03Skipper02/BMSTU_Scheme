;;#1
`number1
(newline)

(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex object)
     (begin
       (write `object)
       (write " => ")
       (let ((result object))       
         (write result)
         (newline)
         result
         )))))

(define (zip . xss)
  (if (or (null? xss) (null? (trace-ex (car xss))))
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))
(zip '(1 2 3) '(one two three))


;;#2
`number2

(load "unit-test.rkt")

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) ; Ошибка здесь!
    (else     1)))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))

(run-tests the-tests)
(newline)


;;#3
`number3
(newline)

(define (ref xs . args)
  (define (loop1 i xs ind)
    (if (null? xs)
        #f
        (if (= i ind)
            (car xs)
            (loop1 (+ i 1) (cdr xs) ind))))
  (define (loop2 i xs ind elem)
    (if (null? xs)
        (if (= i ind)
            (list elem)
            '())
        (if (= i ind)
            (append (cons elem (list (car xs))) (loop2 (+ i 1) (cdr xs) ind elem))
            (cons (car xs) (loop2 (+ i 1) (cdr xs) ind elem)))))
  (define (all->list xs)
    (cond ((string? xs) (string->list xs))
          ((vector? xs) (vector->list xs))
          (else xs)))
  (if (= (length args) 1)
      (loop1 0 (all->list xs) (car args))
      (cond ((vector? xs) (if (and (>= (car args) 0) (>= (vector-length xs) (car args)))
                              (list->vector (loop2 0 (all->list xs) (car args) (cadr args)))
                              #f))
            ((string? xs) (if (and (>= (car args) 0) (>= (string-length xs) (car args)) (char? (cadr args)))
                              (list->string (loop2 0 (all->list xs) (car args) (cadr args)))
                              #f))
            (else (if (and (>= (car args) 0)  (>= (length xs) (car args)))
                      (loop2 0 (all->list xs) (car args) (cadr args))
                      #f)))))


(define the-tests
  (list (test (ref '(1 2 3) 1) 2)
        (test (ref #(1 2 3) 1) 2)
        (test (ref "123" 1) #\2)
        (test (ref "123" 3) #f)
        (test (ref '(1 2 3) 1 0) '(1 0 2 3))
        (test (ref #(1 2 3) 1 0) #(1 0 2 3))
        (test (ref "123" 1 #\0) "1023")
        (test (ref "123" 1 0) #f)
        (test (ref "123" 3 #\4) "1234")
        (test (ref "123" 5 #\4) #f)
        (test (ref "123" -1 #\4) #f)))
(run-tests the-tests)
(newline)

;;#4
`number4
(newline)

(define (factorize form)
  (define sign (car form))
  (define x (ref (ref form 1) 1))
  (define y (ref (ref form 2) 1))
  (define pow (ref (ref form 1) 2))
  (if (equal? pow 2)
      (if (equal? sign '-)
          `(* (- ,x ,y) (+ ,x ,y))
          #f)
      (if (equal? sign '-)
          `(* (- ,x ,y) (+ (+ (expt ,x 2) (expt ,y 2)) (* ,x ,y)))
          `(* (+ ,x ,y) (- (+ (expt ,x 2) (expt ,y 2)) (* ,x ,y))))))

(define the-tests
  (list (test (factorize '(- (expt x 2) (expt y 2))) '(* (- x y) (+ x y)))
        (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2))) '(* (- (+ first 1) (- second 1))
                                                                              (+ (+ first 1) (- second 1))))
        (test (eval (list (list 'lambda 
                                '(x y) 
                                (factorize '(- (expt x 2) (expt y 2))))
                          1 2)
                    (interaction-environment)) -3)
        (test (factorize '(- (expt a 3) (expt b 3))) '(* (- a b) (+ (+ (expt a 2) (expt b 2)) (* a b))))
        (test (factorize '(+ (expt a 3) (expt b 3))) '(* (+ a b) (- (+ (expt a 2) (expt b 2)) (* a b))))
        )
  )

(run-tests the-tests)


;;COUNTER-TESTS + TEST-TESTS

(define counter
  (let ((x 0))
    (lambda ()
      (set! x (+ 1 x))
      x)))

(define counter-tests
  (list
    (test (counter) 1)   ; OK
    (test (counter) 3)   ; FAIL
    (test (counter) 3))) ; OK
(run-tests counter-tests)

(+ (trace-ex (counter)) (trace-ex (counter)))


(define tests-div
  (list
    (test (/ 10 5) 2)
    (test (/ 30 6) 6)
    (test (/ 1 0) 100500)
    (test (/ 1 3) 1/3)))
;(run-tests tests-div)





          












