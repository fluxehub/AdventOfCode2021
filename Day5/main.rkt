#lang racket

(define (parse-line str)
  (let ([parsed (car (regexp-match* #px"(\\d+),(\\d+) -> (\\d+),(\\d+)" str #:match-select cdr))])
    (map string->number parsed)))

(define (find-line-direction x0 y0 x1 y1)
  (cond
    [(= x0 x1) 'Vertical]
    [(= y0 y1) 'Horizontal]
    [else 'Angled]))

; Finds the points along a horizontal or vertical line
; Returns an empty list for lines that are not straight
(define (points-along-line x0 y0 x1 y1)
  (let ([direction (find-line-direction x0 y0 x1 y1)])
    (case direction
      ['Horizontal
       (let ([start (min x0 x1)]
             [end (+ 1 (max x0 x1))])
         (for/list ([x (range start end)])
           (list x y0)))]
      ['Vertical
       (let ([start (min y0 y1)]
             [end (+ 1 (max y0 y1))])
         (for/list ([y (range start end)])
           (list x0 y)))]
      [else '()])))

(define (find-point-intersection-frequencies all-points)
  (let ([frequencies (make-hash)])
    (begin
      (for-each (lambda (points)
                  (for-each (lambda (point)
                              (hash-set! frequencies point (+ 1 (hash-ref frequencies point 0))))
                            points))
                all-points)

      frequencies)))

(define (find-intersected-points input)
  (let* ([lines (map parse-line (file->lines input))]
         [points (map (curry apply points-along-line) lines)]
         [frequencies (hash->list (find-point-intersection-frequencies points))])
    (filter (lambda (frequency) (>= (cdr frequency) 2)) frequencies)))

(define intersected-points (find-intersected-points "input.txt"))

(begin
  (for-each (lambda (point) (printf "~a: ~v\n" (car point) (cdr point))) intersected-points)
  (printf "Count: ~v\n" (length intersected-points)))