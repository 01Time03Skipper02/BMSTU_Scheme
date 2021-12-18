;;#1
(define memoized-factorial
  (let ((known-results '()))
    (lambda (n)
      (let* ((arg (list n))
             (res (assoc arg known-results)))
        (if res
            (cadr res)
            (if (>= n 1)
                (let ((res (* n (memoized-factorial (- n 1)))))
                  (set! known-results (cons (list arg res) known-results))
                  res)
                1))))))
(begin
  (display (memoized-factorial 10)) (newline)
  (display (memoized-factorial 50)) (newline))

;;#2
(define-syntax lazy-cons
  (syntax-rules()
    ((lazy-cons a b) (cons a (delay b)))))

(lazy-cons 4 5)
(force (cdr (lazy-cons 4 5)))
    
(define (lazy-car p)
  (car p))

(lazy-car (lazy-cons 4 5))

(define (lazy-cdr p)
  (force (cdr p)))

(lazy-cdr (lazy-cons 4 5))

(lazy-cdr (lazy-cons 4 (lazy-cons 5 (lazy-cons 6 7))))

(define (lazy-head xs k)
  (define (loop k)
    (if (= k 0)
        '()
        (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))
  (loop k))
(lazy-head (lazy-cons 4 (lazy-cons 5 (lazy-cons 6 7))) 3)

(define (lazy-ref xs k)
  (define (loop k)
    (if (= k 0)
        xs
        (lazy-ref (lazy-cdr xs) (- k 1))))
  (loop k))
(lazy-ref (lazy-cons 4 (lazy-cons 5 (lazy-cons 6 7))) 0)

(define (naturals start)
  (lazy-cons start (naturals (+ 1 start))))

(define (lazy-factorial-generate)
  (define (loop n n!)
    (lazy-cons n! (loop (+ n 1) (* n! (+ n 1)))))
  (loop 1 1))

(define (lazy-factorial n)
  (lazy-car (lazy-ref (lazy-factorial-generate) n)))

(begin
  (display (lazy-factorial 10)) (newline)
  (display (lazy-factorial 50)) (newline))

;;#3
(define (read-words)
  (define (loop xs)
    (let ((x (read-char)))
      (cond ((eof-object? x) xs)
            ((char-whitespace? x) (loop xs))
            (else (loop (append xs (list x)))))))
    
  (loop '()))

;;#4

(define ie (interaction-environment))
(define-syntax define-struct
  (syntax-rules ()
    ((define-struct type (names ...))
     (begin
       (eval (list 'define (string->symbol (string-append (symbol->string 'make-) (symbol->string 'type)))
                   '(lambda (names ... )
                      (list 'type (list (list 'names names ) ...)))) ie)
       (eval (list 'define (string->symbol (string-append (symbol->string 'type) (symbol->string '?)))
                   '(lambda (p)
                      (if (equal? (car p) 'type)
                        #t
                        #f))) ie)
       (eval (list 'define (string->symbol (string-append (symbol->string 'type) (symbol->string '-) (symbol->string 'names)))
                   '(lambda (p)
                      (cadr (assoc 'names (cadr p))))) ie) ...
       (eval (list 'define (string->symbol (string-append (symbol->string 'set) (symbol->string '-) (symbol->string 'type) (symbol->string '-) (symbol->string 'names) (symbol->string '!)))
                   '(lambda (p num)
                     (set-car! (cdr (assoc 'names (cadr p))) num))) ie) ...))))
       
(define-struct pos (row col))
(define p (make-pos 1 2))
(pos? p)
(pos-row p)
(pos-col p)
(set-pos-row! p 3)
(set-pos-col! p 4)
(pos-row p)
(pos-col p)


;;#5
(define (my-eval exprs)
  (eval exprs (interaction-environment)))

(define (s->s s)
  (if (symbol? s)
      (symbol->string s)
      (string->symbol s)))

(define-syntax define-data
  (syntax-rules ()
    ((define-data data-name ((name field1 ...) ...))
     (begin
       (my-eval (list 'define
                      'name
                      (lambda (field1 ...)
                        (list (list 'd-name 'data-name) (list 't-name 'name)
                              (list 'field1 field1) ...)))) ...
       (my-eval (list 'define
                      (s->s (string-append (s->s 'data-name) "?"))
                      (lambda (x)
                        (and (list? x) (>= (length x) 2)
                             (let ((d-nameres (assoc 'd-name x)))
                               (and d-nameres (equal? (cadr d-nameres) 'data-name)))))))))))

(define-syntax match
  (syntax-rules ()
    ((match x ((name field1 ...) expr) ...)
       (cond
         ((equal? (cadadr x) 'name)
           (let ((field1 (cadr (assoc 'field1 x))) ...)
             expr))
          ...
          (else x)))))

; Определяем тип
;
(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

; Определяем значения типа
;
(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))

; Пусть определение алгебраического типа вводит
; не только конструкторы, но и предикат этого типа:
;
(display (and (figure? s)
              (figure? r)
              (figure? t)
              (figure? c))) (newline)

(define pi (acos -1)) ; Для окружности
  
(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))
  
(display (perim s)) (newline)
(display (perim r)) (newline)
(display (perim t)) (newline)
(display (perim c)) (newline)