#lang racket

; --- Day 1 ----

(define dial-positions 100)
(define dial-initial 50)

(define (load-rotations fname)
  (file->list fname))

(define (direction rotation)
  (let ((character (substring (symbol->string rotation) 0 1)))
    (cond ((string=? character "R") 1)
          ((string=? character "L") -1))))

(define (magnitude rotation)
  (string->number (substring (symbol->string rotation) 1)))

(define (check-dial position)
  (if (= 0 position) 1 0))

(define (rotate position rotation)
  (let ((rotation (* (direction rotation) (magnitude rotation))))
    (remainder (+ position rotation) dial-positions)))

(define (rotate-iter current_position password rotations)
  (if (null? rotations)
      password
      (let ((new_position (rotate current_position (car rotations))))
        (rotate-iter new_position (+ password (check-dial new_position)) (cdr rotations)))))

(define (get-password)
  (rotate-iter dial-initial 0 (load-rotations "day1_input.txt")))

(displayln (format "Day 1: Password = ~a" (get-password)))


;; --- Day 2 -----

(define (read-csv-line input)
  (string-split (read-line input) ","))

(define (load-ranges)
  (call-with-input-file "day2_input.txt" read-csv-line))

(define (get-range range)
  (string-split range "-"))

(define (range-begin range)
  (string->number (car (get-range range))))

(define (range-end range)
  (string->number (cadr (get-range range))))

(define (is-invalid-id? id)
  (let* ((str (number->string id))
         (length (string-length str))
         (half (quotient length 2)))
    (string=?
     (substring str 0 half)
     (substring str half length))))

(define (sum-invalids-in-range range)
  (let ((start (range-begin range))
        (end (range-end range)))
    (for/sum ([id (in-inclusive-range start end)])
      (if (is-invalid-id? id) id 0))))

(define (sum-ranges)
  (for/sum ([range (load-ranges)])
    (sum-invalids-in-range range)))

(displayln (format "Day 2: Sum of Invalid IDs = ~a" (sum-ranges)))


;; ------ Day 3 ---------

(define (load-joltage-banks)
  (file->list "day3_input.txt"))

(define (all-but-last-digit bank)
  (quotient bank 10))

(define (get-digits bank)
  (map string->number (map string (string->list (number->string bank)))))

(define (max-first-digit bank)
  (apply max (get-digits (all-but-last-digit bank))))

(define (max-after-first-digit first-digit bank)
  (apply max (cdr (member first-digit (get-digits bank)))))

(define (get-joltage-digits bank)
  (let* ([first-digit
          (max-first-digit bank)]
         [second-digit
          (max-after-first-digit first-digit bank)])
    (list first-digit second-digit)))

(define (digits->number digits)
  (+ (* 10 (car digits)) (cadr digits)))

(define (get-max-joltage bank)
  (digits->number (get-joltage-digits bank)))

(define (get-total-joltage)
  (for/sum ([bank (load-joltage-banks)])
    (get-max-joltage bank)))

(displayln (format "Day 3 - Part 1: Total Output Joltage from banks = ~a" (get-total-joltage)))
