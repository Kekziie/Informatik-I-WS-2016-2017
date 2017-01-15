;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt09-A4-zipWith) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 4
; H.O.P. "zipWith" soll:
;  - eine Funktion f und 2 Listen xs und ys konsumieren
;  - kombiniert Elemente der Listen xs und ys an der gleichen Position mittels funktion f
;  - überstehende Elemente verwerfen
(: zipWith ((%a %b -> %c) (list-of %a) (list-of %b) -> (list-of %c)))