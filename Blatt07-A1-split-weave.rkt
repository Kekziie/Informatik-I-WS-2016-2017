;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt07-A1-split-weave) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1

; (a)
; schreiben einer parametrisch polymorphen Record-Typ für tuple

(define-record-procedures-parametric tuple tuple-of
  make-tuple
  tuple?
  (first
   rest))

; Prozedur "split-list" soll eine Liste in zwei Listen aufspalten
; die Elemente sollen abwechselnd auf beide Ergebnislisten aufgeteilt werden
; bei ungerader Azahl in Eingabeliste -> 1. Ergebnisliste länger

(: split-list ((list-of %a) -> (tuple-of (list-of %a) (list-of %a))))

