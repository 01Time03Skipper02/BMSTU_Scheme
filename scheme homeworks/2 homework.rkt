;;#1.1
(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d))))

(my-range 0 11 3)
(newline)

;;#1.2
(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (list? (car xs))
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs))))))

(my-flatten '((1) 2 (3 (4 5)) 6))
(newline)

;;#1.3
(define (my-element? x xs)
  (if (null? xs)
      #f
      (or (equal? x (car xs)) (my-element? x (cdr xs)))))

(my-element? 1 '(3 2 1))
(my-element? 4 '(3 2 1))
(newline)

;;#1.4
(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs)))))

(my-filter odd? (my-range 0 10 1))
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))
(newline)

;;#1.5
(define (my-fold-left op xs)
  (if (= (length xs) 1)
      (car xs)
      (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs)))))

(my-fold-left  quotient '(16 2 2 2 2))
(my-fold-left  quotient '(1))
(newline)

;;#1.6
(define (my-fold-right op xs)
  (if (<= (length xs) 1)
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

(my-fold-right expt '(2 3 4))
(my-fold-right expt '(2))
(newline)

;;#2.1
(define (list->set xs)
  (if (null? xs)
      '()
      (if (my-element? (car xs) (cdr xs))
          (list->set (cdr xs))
          (append (list->set (cdr xs)) (list (car xs))))))
(list->set '(1 1 2 3)) 
(newline)

;;#2.2
(define (set? xs)
  (if (null? xs)
      #t
      (if (my-element? (car xs) (cdr xs))
          #f
          (set? (cdr xs)))))
(set? '(1 2 3))
(set? '(1 2 3 3))
(set? '())
(newline)

;;#2.3
(define (union xs ys)
  (if (null? xs)
      (reverse ys)
      (if (null? ys)
          (reverse xs)
          (if (my-element? (car xs) ys)
              (union (cdr xs) ys)
              (union (cdr xs) (cons (car xs) ys))))))
(union '(1 2 3) '(2 3 4))
(newline)

;;#2.4
(define (intersection xs ys)
  (if (null? xs)
      '()
      (if (my-element? (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))))
(intersection '(1 2 3) '(2 3 4))
(newline)        
      
;;#2.5
(define (difference xs ys)
  (if (null? xs)
      '()
      (if (my-element? (car xs) ys)
          (difference (cdr xs) ys)
          (cons (car xs) (difference (cdr xs) ys)))))
(difference '(1 2 3 4 5) '(2 3))
(newline)

;;#2.6
(define (symmetric-difference xs ys)
  (union (reverse (difference xs ys)) (difference ys xs)))

(symmetric-difference '(1 2 3 4) '(3 4 5 6))
(newline)

;;#2.7
(define (set-eq? xs ys)
  (null? (symmetric-difference xs ys)))

(set-eq? '(1 2 3) '(3 2 1))                  
(set-eq? '(1 2) '(1 3))
(newline)

;;#3.1
(define (string-trim-left str)
  (if (char-whitespace? (string-ref str 0))
      (string-trim-left (substring str 1))
      str))
(string-trim-left  "\t\tabc def")
(newline)

;;#3.2
(define (str-reverse str)
  (list->string (reverse (string->list str))))
(define (string-trim-right str)
  (str-reverse (string-trim-left (str-reverse str))))
(string-trim-right "abc def\t")
(newline)

;;#3.3
(define (string-trim str)
  (string-trim-right (string-trim-left str)))
(string-trim       "\t abc def \n")
(newline)

;;#3.4
(define (string-prefix? a b)
  (cond ((> (string-length a) (string-length b)) #f)
        ((equal? a (substring b 0 (string-length a))) #t)
        (else #f)))
(string-prefix? "abc" "abcdef") 
(string-prefix? "bcd" "abcdef")  
(string-prefix? "abcdef" "abc")
(newline)

;;#3.5
(define (string-suffix? a b)
  (cond ((> (string-length a) (string-length b)) #f)
        ((equal? a (substring b (- (string-length b) (string-length a)))) #t)
        (else #f)))
(string-suffix? "def" "abcdef")  
(string-suffix? "bcd" "abcdef")
(newline)

;;#3.6
(define (string-infix? a b)
  (cond ((> (string-length a) (string-length b)) #f)
        ((string-prefix? a b) #t)
        (else (string-infix? a (substring b 1)))))
(string-infix? "def" "abcdefgh") 
(string-infix? "abc" "abcdefgh") 
(string-infix? "fgh" "abcdefgh") 
(string-infix? "ijk" "abcdefgh") 
(string-infix? "bcd" "abc")
(newline)

;;#3.7
(define (obje str sep)
  (if (or (string-prefix? sep str) (= (string-length str) 0))
      ""
      (string-append (make-string 1 (string-ref str 0)) (obje (substring str 1) sep))))

(define (string-split str sep)
  (if (= (string-length str) 0)
      '()
      (if (string-prefix? sep str)
          (string-split (substring str (string-length sep)) sep)
          (cons (obje str sep) (string-split (substring str (string-length (obje str sep))) sep)))))
(string-split "x;y;z" ";")
(string-split "x-->y-->z" "-->")
(string-split "abc;def;ghi" ";")
(newline)


;;#4.1




  




;;#5
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

