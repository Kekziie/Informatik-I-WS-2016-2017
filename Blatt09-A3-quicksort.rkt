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