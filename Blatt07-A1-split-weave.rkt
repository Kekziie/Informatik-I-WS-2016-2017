;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt07-A1-split-weave) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1

; (a)
; schreiben einer parametrisch polymorphen Record-Typ für tuple

(define-record-procedures-parametric tuple tuple-of
  make-tuple
  tuple?
  (tuple-first
   tuple-rest))

; Prozedur "split-list" soll eine Liste in zwei Listen aufspalten
; die Elemente sollen abwechselnd auf beide Ergebnislisten aufgeteilt werden
; bei ungerader Azahl in Eingabeliste -> 1. Ergebnisliste länger

;(: split-list ((list-of %a) -> (tuple-of (list-of %a) (list-of %a))))

;(check-expect (split-list (list 1 2 3 4 5)) (make-tuple (list 1 3 5) (list 2 4)))
;(check-expect (split-list empty-list) (make-tuple empty-list empty-list))
;(check-expect (split-list (list 3)) (make-tuple (list 3) empty-list))
;(check-expect (split-list (list "Samstag" "Sonntag")) (make-tuple (list "Samstag") (list "Sonntag")))

;(define split-list
;  (lambda (xs)
;    (cond
;      ((empty? xs) (make-tuple empty-list empty-list))
;      ((empty? (rest xs)) (make-tuple (list (first xs)) empty-list))
;      (else (make-tuple (list (first xs) (first (rest (rest xs))) (split-list (rest (rest xs))))
;                        (list (first (rest xs)) (split-list (rest xs))))))))

; (b)

; Prozedur "weave-lists" soll zwei Listen in eine Liste verschmelzen
; Ergebnisliste enthält Elemente der konsumierten Liste in abwechselnder Reihenfolge
; bei ungleicher Länge der Listen -> überschüssige Elemente werden an Ergebnisliste hinten angehängt

(: weave-list ((tuple-of (list-of %a) (list-of %a)) -> (list-of %a)))


