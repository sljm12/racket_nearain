#lang racket
(require net/http-easy)
(require racket/date)

(define res
    (get "https://example.com"))

(define(test-url url)
  (let ([r (head url)])
    (= (response-status-code r) 200)
    ))

(define (closest-factor value factor)
  (- value (modulo value factor)))

(define (closest-5-min dateStruct)
  (reduce-time-by dateStruct (minutes->seconds (modulo (date-minute dateStruct) 5))))

(define (make-rain-url date)
  (let ([floordate (make-dateTime-String (reduce-time-by date (minutes->seconds 5)))])
  (string-append "https://www.nea.gov.sg/docs/default-source/rain-area/dpsri_70km_" floordate "0000dBR.dpsri.png")))

(define (format-number v) (~a v #:width 2 #:left-pad-string "0" #:align `right))

(define (make-dateTime-String datetime)
  (date-display-format `iso-8601)
  
  (apply string-append (append (list (string-replace (date->string datetime) "-" ""))
                               (map format-number (list (date-hour datetime) (date-minute datetime))))))

(define (reduce-time-by datestruct seconds)
  (seconds->date (- (date->seconds datestruct) seconds)))

(define (minutes->seconds x)
  (* x 60))

(define (generate-rain-url number)
  (for/list ([i (range number)])
    (let* ([mindelta (* i 5)]
          [date (current-date)]
          [reduce-date (closest-5-min (reduce-time-by date (minutes->seconds mindelta)))])
      
      (make-rain-url reduce-date)
     )))

(make-dateTime-String (current-date))

;(define (get-rain-url)
;  (let ([current-date (current-date)]
;        [found #f])
;    (while (= found #f))))
           
    