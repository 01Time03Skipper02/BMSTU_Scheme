;;test for #4 hw2
;список с координатой 
(define (make-list xs)
  (if (null? xs)
      '()
      (cons 0 (make-list (cdr xs)))))
;получение координаты следующего элемента
(define (nextnum num xs)
  (if (null? num)
      '()
      (if (< (car num) (- (car xs) 1))
          (cons (+ (car num) 1) (cdr num))
          (cons 0 (nextnum (cdr num) (cdr xs))))))
;сам мультувектор
(define (make-multi-vector . xs)
  (define sizes (car xs))
  (define fillnum (if (null? (cdr xs))
                      0
                      (cadr xs)))
  (define numb (make-list (car xs)))
  (define (loop i n)
    (if (= i n)
        '()
        (if (= i 0)
            (cons (cons fillnum (list (reverse numb))) (loop (+ i 1) n))
            (begin (set! numb (nextnum numb (reverse sizes))) (cons (cons fillnum (list (reverse numb))) (loop (+ i 1) n))))))
  (list->vector (loop 0 (apply * sizes))))
(define m (make-multi-vector '(3 3 3)))

;;4.2
(define (multi-vector? m)
  (list? (vector-ref m 0)))
(multi-vector? m)

;;4.3
(define (vector-size m)
  (length (vector->list m)))
(define (multi-vector-ref m numb)
  (define (loop i n)
    (cond ((= i n) #f)
          ((equal? (car (cdr (vector-ref m i))) numb) (car (vector-ref m i)))
          (else (loop (+ i 1) n))))
  (loop 0 (vector-size m)))
(multi-vector-ref m '(1 0 0))
(newline)

(define (multi-vector-set! m numb elem)
  (define (loop i n)
    (cond ((= i n) #f)
          ((equal? (car (cdr (vector-ref m i))) numb) (vector-set! m i (cons elem (list numb))))
          (else (loop (+ i 1) n))))
  (loop 0 (vector-size m)))
m
(newline)
(multi-vector-ref m '(1 0 0))
(newline)
(multi-vector-set! m '(1 0 0) 5)
(multi-vector-ref m '(1 0 0))
(newline)
m






