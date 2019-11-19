#lang racket/base

(require racket/match
         plot)

(define count-pair-contests 100)
(define num-players 50)
(define eps 0.0001)

(define (generate-skills count)
  (define skill (make-vector count 0))

  (define (replace i sum)
    (define old (vector-ref skill i))
    (define new (random))
    (vector-set! skill i new)
    (+ (- sum old) new))

  (define (fill-until-average i sum)
    (when (> (abs (- 0.5 (/ sum count))) eps)
      (fill-until-average (modulo (add1 i) count)
                          (replace i sum))))
  (fill-until-average 0 0)
  skill)

(define player-skill
  (generate-skills num-players))

(define (pvp a b)
  (define a-skill (vector-ref player-skill a))
  (define b-skill (vector-ref player-skill b))
  (define p-a (/ a-skill (+ a-skill b-skill)))
  (if (< (random) p-a) (values a b) (values b a)))

(define (record-outcome! observed-wins win/loss-f player)
  (match-define (cons wins contests)
    (vector-ref observed-wins player))
  (vector-set! observed-wins player
               (cons (win/loss-f wins) (add1 contests))))
  
(define (record-win! observed-wins a)
  (record-outcome! observed-wins add1 a))

(define (record-loss! observed-wins a)
  (record-outcome! observed-wins values a))

(define (contest! observed-wins a b)  
  (define-values (winner loser) (pvp a b))
  (record-win! observed-wins winner)
  (record-loss! observed-wins loser))

(define (run-simulation)
  (define observed-wins
    (make-vector num-players (cons 0 0)))
  (define num-contests (* count-pair-contests num-players num-players))
  (for ([_x (in-range num-contests)])
    (contest! observed-wins
              (random num-players)
              (random num-players)))
  observed-wins)

(define (run-simulation2)
  (define observed-wins
    (make-vector num-players (cons 0 0)))
  (for* ([_x (in-range count-pair-contests)]
         [a  (in-range num-players)]
         [b  (in-range num-players)])
    (contest! observed-wins a b))
  observed-wins)

(define (plot-obs-ubs wins)
  (define (scatter x color)
    (points
     #:color color
     (for/list ([skill (in-vector player-skill)]
                [wins  (in-vector x)])
       (list (/ (car wins) (cdr wins)) skill))))
  (plot #:x-min 0 #:x-max 1
        #:y-min 0 #:y-max 1
        #:x-label "observed"
        #:y-label "skill"
        (list         
         (scatter wins 'blue)        
         (function
          (lambda (x) (/ x (* 2 (- 1 x))))))))