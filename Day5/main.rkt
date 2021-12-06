#lang racket

(define input "input.txt")

(define (get-lines input) (map parse-line (file->lines input)))

(define (parse-line str)
  (let ([parsed (regexp-match* #px"(\\d+)" str)])
    (map string->number parsed)))

(define (find-intersection-frequencies frequencies x0 y0 x1 y1)
  (let* ([dx (sgn (- x1 x0))]
         [dy (sgn (- y1 y0))]
         [xs (if (zero? dx) (make-list (+ 1 (abs (- y0 y1))) x0) (inclusive-range x0 x1 dx))]
         [ys (if (zero? dy) (make-list (+ 1 (abs (- x0 x1))) y0) (inclusive-range y0 y1 dy))])
                (for-each (lambda (point)
                            (hash-set! frequencies point (+ 1 (hash-ref frequencies point 0))))
                          (map list xs ys))))

(define (straight? x0 y0 x1 y1) (or (= x0 x1) (= y0 y1)))

(define (count-intersections lines)
  (let ([frequencies (make-hash)])
    (begin
      (for-each (curry apply (curry find-intersection-frequencies frequencies)) lines)
      (length (filter (lambda (frequency) (>= (cdr frequency) 2)) (hash->list frequencies))))))

(printf "Part 1 count: ~v\n" (count-intersections (filter (curry apply straight?) (get-lines input))))
(printf "Part 2 count: ~v\n" (count-intersections (get-lines input)))