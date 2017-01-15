;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt09-A4-zipWith) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 4
; H.O.P. "zipWith" soll:
;  - eine Funktion f und 2 Listen xs und ys konsumieren
;  - kombiniert Elemente der Listen xs und ys an der gleichen Position mittels funktion f
;  - überstehende Elemente verwerfen
(: zipWith ((%a %b -> %c) (list-of %a) (list-of %b) -> (list-of %c)))

(check-expect (zipWith +
                       (list 1 2 3)
                       (list 10 20 30 40)) (list 11 22 33))
(check-expect (zipWith *
                       empty
                       (list 1 2 3)) empty)
(check-expect (zipWith /
                       empty
                       empty) empty)
(check-expect (zipWith +
                       (list 42)
                       empty) empty)
(check-expect (zipWith *
                       (list 1 2 3)
                       (list 3 2 1)) (list 3 4 3))
(check-within (zipWith /
                       (list 2 4)
                       (list 3)) (list 2/3) 0.001)

(define zipWith
  (lambda (f xs ys)
    (cond
      ((or (empty? xs)
           (empty? ys)) empty)
      ((and (pair? xs)
            (pair? ys)) (make-pair (f (first xs) (first ys))
                                   (zipWith f (rest xs) (rest ys)))))))