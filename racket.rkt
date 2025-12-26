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

(define (solve-1)
  (displayln (format "Day 1: Password = ~a" (get-password))))


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

(define (solve-2)
  (displayln (format "Day 2: Sum of Invalid IDs = ~a" (sum-ranges))))


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

;;; Day 3 Part 2
;;; Using 12 digits instead of 2

(define (load-joltage-banks-2)
  (map get-digits (load-joltage-banks)))

(define (all-but-last-n-digits bank n)
  (take bank (- (length bank) n)))

(define (max-digit-n bank total-digits-left-to-use)
  (apply max (all-but-last-n-digits bank (sub1 total-digits-left-to-use))))

(define (remaining-digits-after value bank)
  (cdr (member value bank)))

(define (get-digit-value digit position)
  (* digit (expt 10 (sub1 position))))

(define (find-digits-iter digits number-of-digits-left total)
  (if (or (= number-of-digits-left 0) (null? digits))
      total
      (let ([current-digit (max-digit-n digits number-of-digits-left)])
        (find-digits-iter (remaining-digits-after current-digit digits)
                          (sub1 number-of-digits-left)
                          (+ total (get-digit-value current-digit number-of-digits-left))))))

(define (get-max-joltage-2 bank total-digits)
  (find-digits-iter bank total-digits 0))

(define (get-total-joltage-2)
  (for/sum ([bank (load-joltage-banks-2)])
    (get-max-joltage-2 bank 12)))

(define (solve-3)
  (displayln (format "Day 3: (Part 1) Total Output Joltage from banks = ~a" (get-total-joltage)))
  (displayln (format "Day 3: (Part 2) Total Output Joltage from banks = ~a" (get-total-joltage-2))))


;; Day 4

(define (load-rolls)
  (map string->list (map symbol->string (file->list "day4_input.txt"))))

(define (make-position row col)
  (list row col))

(define (get-row position)
  (car position))

(define (get-col position)
  (cadr position))

(define (get-char position rolls)
  (list-ref (list-ref rolls (get-row position)) (get-col position)))

(define (is-roll? position rolls)
  (let ([char (get-char position rolls)])
    (equal? char #\@)))

(define (check-position position rolls)
  (let ([row (get-row position)]
        [col (get-col position)])
    (cond
      [(or (< row 0) (>= row (length rolls))) 0]
      [(or (< col 0) (>= col (length (car rolls)))) 0]
      [else (if (is-roll? position rolls) 1 0)])))

(define (adjacent-positions row col)
  (list (make-position (sub1 row) (sub1 col))
        (make-position (sub1 row) col)
        (make-position (sub1 row) (add1 col))
        (make-position row (sub1 col))
        (make-position row (add1 col))
        (make-position (add1 row) (sub1 col))
        (make-position (add1 row) col)
        (make-position (add1 row) (add1 col))))

(define (count-adjacent-rolls position rolls)
  (let ([row (get-row position)]
        [col (get-col position)])
    (for/sum ([position (adjacent-positions row col)])
      (check-position position rolls))))

(define (count-roll position rolls min)
  (if (not (is-roll? position rolls))
      0
      (if (< (count-adjacent-rolls position rolls) min) 1 0)))

(define (count-row row rolls min)
  (for/sum ([col (in-range 0 (length (list-ref rolls row)))])
    (count-roll (make-position row col) rolls min)))

(define (count-total-rolls rolls min)
  (for/sum ([row (in-range 0 (length rolls))])
    (count-row row rolls min)))

;;;; Day 4 Part 2

(define (move-char position rolls min)
  (if (= (count-roll position rolls min) 1)
      #\x
      (get-char position rolls)))

(define (process-row row rolls min)
  (for/list ([col (in-range 0 (length (list-ref rolls row)))])
    (move-char (make-position row col) rolls min)))

(define (process-grid rolls min)
  (for/list ([row (in-range 0 (length rolls))])
    (process-row row rolls min)))

(define (process-iter rolls min updated-grid)
  (if (equal? rolls updated-grid)
      updated-grid
      (process-iter updated-grid min (process-grid updated-grid min))))

;; Process until the grid stops changing
(define (process-all rolls min)
  (process-iter rolls min (process-grid rolls min)))

(define (is-removed? char)
  (equal? char #\x))

(define (count-row-2 row)
  (count is-removed? row))

(define (count-total-removed rolls min)
  (let ([processed (process-all rolls min)])
    (for/sum ([row processed])
      (count-row-2 row))))

(define (solve-4)
  (displayln (format "Day 4: (Part 1) Total available rolls with one pass= ~a" (count-total-rolls (load-rolls) 4)))
  (displayln (format "Day 4: (Part 2) Total available rolls = ~a" (count-total-removed (load-rolls) 4))))

;; Day 5

(define (load-ranges-data)
  (map symbol->string (file->list "day5_input_ranges.txt")))

(define (load-ranges-id)
  (map string->range (load-ranges-data)))

(define (load-ids)
  (file->list "day5_input_values.txt"))

(define (make-range min max)
  (list min max))

(define (min-range rng)
  (car rng))

(define (max-range rng)
  (cadr rng))

(define (enumerate-range rng)
  (range (min-range rng) (add1 (max-range rng))))

(define (string->range str)
  (let ([vals (string-split str "-")])
    (apply make-range (map string->number vals))))

(define (inspect-ingredient id ranges)
  (if (is-in-ranges id ranges) 1 0))

(define (is-in-range id rng)
  (and (>= id (min-range rng)) (<= id (max-range rng))))

(define (is-in-ranges id ranges)
  (cond [(null? ranges) #f]
        [(is-in-range id (car ranges)) #t]
        [else (is-in-ranges id (cdr ranges))]))

(define (get-fresh-ingredients)
  (for/sum ([id (load-ids)])
    (inspect-ingredient id (load-ranges-id))))

;; Day 5 Part 2

(define (sort-ranges ranges)
  (sort ranges < #:key min-range))

(define (merge-ranges a b)
  (make-range (apply min (map min-range (list a b))) (apply max (map max-range (list a b)))))

(define (is-overlap a b)
  (>= (max-range a) (sub1 (min-range b))))

(define (try-merge current rest)
  (let ([last-merged (car rest)])
    (if (is-overlap last-merged current)
        (cons (merge-ranges last-merged current) (cdr rest))
        (cons current rest))))

(define (merge-sorted-ranges ranges)
  (if (or (null? ranges) (null? (cdr ranges))) ranges
      (let ([first (car ranges)]
            [second (cadr ranges)]
            [rest (cddr ranges)])
        (if (is-overlap first second)
            (merge-sorted-ranges (cons (merge-ranges first second) rest))
            (cons first (merge-sorted-ranges (cons second rest)))))))

;; An alternative implementation using fold
(define (merge-sorted-ranges-fold sorted-ranges)
  (if (null? sorted-ranges)
      '()
      (reverse
       (foldl try-merge
              (list (car sorted-ranges))
              (cdr sorted-ranges)))))

;; An alternative implemention using for/fold
(define (merge-sorted-ranges-for-fold sorted-ranges)
  (if (null? sorted-ranges)
      '()
      (reverse
       (for/fold ([merged-ranges (list (car sorted-ranges))])
                 ([current (cdr sorted-ranges)])
         (if (is-overlap (car merged-ranges) current)
             (cons (merge-ranges (car merged-ranges) current) (cdr merged-ranges))
             (cons current merged-ranges))))))

(define (merge-all-ranges ranges)
  (merge-sorted-ranges-for-fold (sort-ranges ranges)))

(define (count-range rng)
  (add1 (- (max-range rng) (min-range rng))))

(define (count-distinct-ids ranges)
  (for/sum ([rng (merge-all-ranges ranges)])
    (count-range rng)))

(define (solve-5)
  (displayln (format "Day 5: (Part 1) Total fresh ingredients = ~a" (get-fresh-ingredients)))
  (displayln (format "Day 5: (Part 2) Total fresh ingredient IDs available = ~a" (count-distinct-ids (load-ranges-id)))))


;; Day 6
;;; Part 1

(define (split-lines lines)
  (map string-trim (string-split lines "\n")))

(define (split-elements lines)
  (map string-split lines))

(define (load-problems)
  (split-elements (split-lines (file->string "day6_input.txt"))))

(define (transpose data)
  (apply map list data))

(define (get-matrix)
  (transpose (load-problems)))

(define (convert-types lst)
  (if (null? (cdr lst))
      (list (string->symbol (car lst)))
      (cons (string->number (car lst)) (convert-types (cdr lst)))))

(define (calculate-eqn eqn)
  (eval (reverse (convert-types eqn))))

(define (calculate-sum)
  (for/sum ([eqn (get-matrix)])
    (calculate-eqn eqn)))

(displayln (format "Day 6: (Part 1) Total sum of equations = ~a" (calculate-sum)))
