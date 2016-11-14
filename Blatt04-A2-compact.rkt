;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt04-A2-compact) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 2

(: less-zero? (number -> boolean))
(define less-zero?
  (lambda (x)
    (if (< x 0)
        #t
        #f)))

(: f (number -> boolean))
(define f
  (lambda (y)
    (if (< y 11)
        #f
        #t)))
                
(: g (boolean boolean -> boolean))
(define g
  (lambda (a b)
     (not b)))

(: greater-equal-zero? (number -> boolean))
(define greater-equal-zero?
  (lambda (x)
       (if (< x 0)
           #f
           #t)))