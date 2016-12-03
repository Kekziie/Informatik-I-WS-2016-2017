;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt02-A1-float) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
;Aufgabe 1

;maximaler Unterschied zweier Fliesskommazahlen a und b von einem Differenzwert eps
; die Ergebniswerte sollen entweder #t oder #f sein
(: equal-eps (real real real -> boolean))
(check-expect (equal-eps 0.5 0.49999999999 0.1)#t)
(check-expect (equal-eps 1.3 1.38574643 0.1)#t)
(check-expect (equal-eps -5.5 -5.733457027503 0.0001)#f)
(check-expect (equal-eps 19.5 8.9 0.1)#f)
(check-expect (equal-eps 4.53532 4.54442 0.05)#t)

(define equal-eps
  (lambda (a b eps)
    (<= (abs (- a b)) eps)))
