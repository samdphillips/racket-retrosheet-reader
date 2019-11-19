#lang racket/base

(require racket/match
         racket/string
         file/unzip

         csv-reading
         
         rebellion/base/variant
         rebellion/collection/entry
         rebellion/collection/record
         rebellion/streaming/transducer
         rebellion/streaming/reducer)

(define event-file-path
  (string->path "C:/Users/sam/Downloads/2013eve.zip"))

(define (experiment1)
  (call-with-input-file event-file-path
    (lambda (an-input-port)
      (unzip an-input-port
             (lambda (extract-path-bytes dir? extract-input-port)
               (unless dir?
                 (when (bytes=? #"2013OAK.EVA" extract-path-bytes)
                   (for ([x (in-port read-line extract-input-port)]
                         [_i 200])
                     (when (string-prefix? x "start")
                       (display x))))))))))

(define decode-count
  (let ([zero (char->integer #\0)])
    (lambda (i)
      (lambda (s)
        (- (char->integer (string-ref s i)) zero)))))

(define count-balls   (decode-count 0))
(define count-strikes (decode-count 1))

(define fields->event
  (match-lambda
    [(list "id" id)
     (variant #:game-id id)]
    [(list "info" key value)
     (variant #:game-info (entry key value))]
    [(list "start" player-id player-name team batting-pos field-pos)
     (variant #:starting-lineup
              (record #:player-id (string->symbol player-id)
                      #:player-name player-name
                      #:team (if (string=? "0" team) 'away 'home)
                      #:batting-pos (string->number batting-pos)
                      #:field-pos (string->number field-pos)))]
    [(list "play" inning team player-id count pitches play)
     (variant #:play
              (record #:inning (string->number inning)
                      #:team (if (string=? "0" team) 'away 'home)
                      #:player-id (string->symbol player-id)
                      #:count-balls (count-balls count)
                      #:count-strikes (count-strikes count)
                      #:pitches pitches
                      #:play play))]
    [fields fields]))

(define (process-file an-input-port)
  (define reader
    (make-csv-reader an-input-port))
  (transduce (in-producer reader)
             (taking 1000)
             (mapping fields->event)
             (filtering
              (lambda (x)
                (and (variant? x)
                     (variant-tagged-as? x '#:play))))
             #:into
             (into-for-each writeln)))

(define (run)
  (call-with-input-file event-file-path
    (lambda (zip-input-port)
      (define a-zip-directory (read-zip-directory zip-input-port))
      (unzip-entry zip-input-port
                   a-zip-directory
                   #"2013OAK.EVA"
                   (lambda (path-bytes dir? data-input-port)
                     (process-file data-input-port))))))
