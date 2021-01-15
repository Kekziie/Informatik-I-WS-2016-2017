;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt09-A3-quicksort) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 3

; H.O.P. "filter" extrahiert die Elemente xs, die das Prädikat p? erfüllen
(: filter ((%a -> boolean) (list-of %a) -> (list-of %a)))

(check-expect (filter natural? empty) empty)
(check-expect (filter natural? (list 1 2 3)) (list 1 2 3))
(check-expect (filter natural? (list 1 "a" 2 "b" 3)) (list 1 2 3))
(check-expect (filter integer? (list 1/2 0 -1 42 #f)) (list 0 -1 42))

(define filter
  (lambda (p? xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (if (p? (first xs))
                      (make-pair (first xs)
                                 (filter p? (rest xs)))
                      (filter p? (rest xs)))))))

; Prozedur "quicksort" soll:
; - eine Liste xs von Zahlen akzeptieren
; - leere Liste -> sortiert
; - nichte leere Liste:
;    1.) Pivot-Element x bestimmen -> erstes Element der Liste
;    2.) Aufteilung der Liste xs in 2 Listen: 1. Elemente kleiner gleich x, 2. Elemente größer x
;    3.) Sortierung der beiden Listen rekursiv
;    4.) Zusammenfügung der soriterten Liste mit Pivot-Element in der Mitte
(: quicksort ((list-of number) -> (list-of number)))

(check-expect (quicksort (list 0 -5 5 -10 10)) (list -10 -5 0 5 10))
(check-expect (quicksort empty) empty)
(check-expect (quicksort (list 1)) (list 1))
(check-expect (quicksort (list 1 2 3)) (list 1 2 3))
(check-expect (quicksort (list 5 3 1)) (list 1 3 5))
(check-within (quicksort (list 1/2 2 -2.5 -10 8 1/2 4 13 42)) (list -10 -2.5 1/2 1/2 2 4 8 13 42) 0.001)

(define quicksort
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (append (quicksort (filter (lambda (x)
                                                   (<= x (first xs))) (rest xs)))
                          (list (first xs))
                          (quicksort (filter (lambda (x)
                                                   (not (<= x (first xs)))) (rest xs))))))))