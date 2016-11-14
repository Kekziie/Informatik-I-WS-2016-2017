;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt02-A2-temperature) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
;(a.)
;umrechnen von Celsius (C) in Fahrenheit (F)

(:Celsius-in-Fahrenheit (real->real))

(check-expect (Celsius-in-Fahrenheit 28)82.4)
(check-expect (Celsius-in-Fahrenheit -5)23)
(check-expect (Celsius-in-Fahrenheit 0)32)
(check-within (Celsius-in-Fahrenheit 1)33.8 0.1)

(define Celsius-in-Fahrenheit
  (lambda (C) (+(*9/5C)
                32)))

;(b.)
;umrechnen von Fahrenheit (F) in Celsius (C)

(:Fahrenheit-in-Celsius (real->real))

(check-expect (Celsius-in-Fahrenheit 0)-18)
(check-expect (Celsius-in-Fahrenheit 32)0)
(check-expect (Celsius-in-Fahrenheit -4)-20)
(check-within (Celsius-in-Fahrenheit 4)-15.6 0.1)


define Fahrenheit-in-Celsius
(lambda (F) (-(*5/9 Fahrenheit)
              (160/9)))
