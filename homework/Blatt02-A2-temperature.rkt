;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt02-A2-temperature) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 2

;(a.)
;umrechnen von Celsius (C) in Fahrenheit (F)

(: Celsius-in-Fahrenheit (real -> real))

(check-within (Celsius-in-Fahrenheit 28)82.4 0.01)
(check-expect (Celsius-in-Fahrenheit -5)23)
(check-expect (Celsius-in-Fahrenheit 0)32)
(check-within (Celsius-in-Fahrenheit 1)33.8 0.1)

(define Celsius-in-Fahrenheit
  (lambda (C) (+ (* 9/5 C)
                 32)))

;(b.)
;umrechnen von Fahrenheit (F) in Celsius (C)

(: Fahrenheit-in-Celsius (real -> real))

(check-expect (Fahrenheit-in-Celsius 32) 0)
(check-within (Fahrenheit-in-Celsius 0) -17.78 0.01)
(check-expect (Fahrenheit-in-Celsius 5) -15)
(check-within (Fahrenheit-in-Celsius -5.5) -20.83 0.01)


(define Fahrenheit-in-Celsius
(lambda (F) (* (- F 32)
               5/9)))
