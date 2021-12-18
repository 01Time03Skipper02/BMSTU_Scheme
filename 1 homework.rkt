;;#1
(define (day-of-week day month year)
  (< month 3)
  (remainder (+ day (quotient (* 31 (+ month 10)) 12) (- year 1) (- (quotient (- year 1) 4) (quotient (- year 1) 100)) (quotient (- year 1) 400)) 7)
  (remainder (+ day (quotient (* 31 (- month 2)) 12) year (- (quotient year 4) (quotient year 100)) (quotient year 400)) 7))

(day-of-week 15 09 2021)

;;#2
(define (diskr a b c)
  (- (* b b) (* 4 a c))
  )
(define (uravn a b c)
  (if (= a 0)
      (if (= b 0)
          (list)
          (list (/ (* -1 c) b)))
      (if (< (diskr a b c) 0)
          (list)
          (if (= (diskr a b c) 0)
              (list (* -1 (/ b (* a 2))))
              (list (/ (+ (* -1 b) (sqrt (diskr a b c))) (* a 2)) (/ (- (* -1 b) (sqrt (diskr a b c))) (* a 2)))
              )
          )
      ))
                 
(uravn 4 20 25)
(uravn 0 0 5)
(uravn 3 14 5)

;;#3
(define (my-gcd a b)
  (if (= b 0)
      a
      (my-gcd b (remainder a b))))

(my-gcd 12 6)

;;#4
(define (my-lcm a b)
  (quotient (* a b) (my-gcd a b)))

(my-lcm 13 7)

;;#5
(define  (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (prime? n)
  (= (remainder (+ (factorial (- n 1)) 1) n) 0))
(prime? 13)
  


  


  
  








