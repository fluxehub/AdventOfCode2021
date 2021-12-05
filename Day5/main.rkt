#lang racket

(struct point (x y) 
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc point-val output-port output-mode)
    (fprintf output-port "(~a, ~a)" (point-x point-val) (point-y point-val)))])

(struct line (point0 point1)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc line-val output-port output-mode)
    (fprintf output-port "~a -> ~a" (line-point0 line-val) (line-point1 line-val)))])

; I could have done regex but this works as well
(define (parse-point str) 
  (let* ([split (string-split str ",")]
         [as-number (map string->number split)]) 
    (apply point as-number)))

; Returns a line as values x0 y0 x1 y1
(define (unwrap-line line)
  (let ([x0 (point-x (line-point0 line))]
        [y0 (point-y (line-point0 line))]
        [x1 (point-x (line-point1 line))]
        [y1 (point-y (line-point1 line))])
    (values x0 y0 x1 y1)))

(define (parse-line str)
  (let* ([points (string-split str " -> ")]
         [parsed (map parse-point points)]) 
    (apply line parsed)))

(define (find-line-direction line)
  (let-values ([(x0 y0 x1 y1) (unwrap-line line)])
    (cond
      [(= x0 x1) 'Vertical]
      [(= y0 y1) 'Horizontal]
      [else 'Angled])))

; Finds the points along a horizontal or vertical line
; Returns an empty list for lines that are not straight
(define (points-along-line line)
  (let-values ([(x0 y0 x1 y1) (unwrap-line line)])
    (let ([direction (find-line-direction line)])
      (case direction
        ['Horizontal
          (let ([start (min x0 x1)]
                [end (+ 1 (max x0 x1))])
            (for/list ([x (range start end)])
              (point x y0)))]
        ['Vertical
          (let ([start (min y0 y1)]
                [end (+ 1 (max y0 y1))])
            (for/list ([y (range start end)])
              (point x0 y)))]
        [else '()]))))

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
         [points (map points-along-line lines)]
         [frequencies (hash->list (find-point-intersection-frequencies points))])
    (filter (lambda (frequency) (>= (cdr frequency) 2)) frequencies)))

(define intersected-points (find-intersected-points "input.txt"))

(begin
  (for-each (lambda (point) (printf "~a: ~v\n" (car point) (cdr point))) intersected-points)
  (printf "Count: ~v\n" (length intersected-points)))
