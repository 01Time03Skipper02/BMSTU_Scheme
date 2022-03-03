;;#1
(define (count x xs)
  (if (null? xs)
      0
      (if (equal? x (car xs))
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs)))))
(count 'a '(a b s a h a a))


;;#2
(define (delete pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (cons (car xs) (delete pred? (cdr xs))))))
(delete odd? '(1 2 4 6))
(delete even? '(2 4 6 6 8 9 9))


;;#3
(define (iterate f x n)
  (if (= n 0)
      '()
      (cons x (iterate f (f x) (- n 1)))))
(iterate (lambda (x) (* 2 x)) 2 10)
(iterate (lambda (x) (/ x 3)) 81 4)


;;#4
(define (intersperse e xs)
  (if (null? xs)
      '()
      (if (= (length xs) 1)
          (cons (car xs) (intersperse e (cdr xs))) 
          (cons (car xs) (cons e (intersperse e (cdr xs)))))))
(intersperse 'x '(1 2 3 4))
(intersperse 'x '(1 2 3 4 5 6))


;;#5.1
(define (any? pred? xs)
  (if (null? xs)
      #f
      (or (pred? (car xs)) (any? pred? (cdr xs)))))
(any? odd? '(1 3 5 7)) 
(any? odd? '(0 1 2 3)) 
(any? odd? '(0 2 4 6)) 
(any? odd? '())

;;#5.2
(define (all? pred? xs)
  (if (null? xs)
      #t
      (and (pred? (car xs)) (all? pred? (cdr xs)))))
(all? odd? '(1 3 5 7)) 
(all? odd? '(0 1 2 3)) 
(all? odd? '(0 2 4 6)) 
(all? odd? '())

;;#6
(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xs)
  (lambda (x)
    (define (seco xs x)
      (if (null? xs)
          x
          (seco (cdr xs) ((car xs) x))))
    (seco (reverse xs) x)))

((o f g h) 1)
((o f g) 1)
((o h) 1)
((o) 1)

      
      
  



