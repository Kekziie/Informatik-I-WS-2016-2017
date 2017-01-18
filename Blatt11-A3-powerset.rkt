;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt11-A3-powerset) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 3

; Potenzmenge P(S) einer Menge S ist die Menge aller Teilmengen von S
; P(S) enthält dabei immer die leere Menge Ø sowie S selbst
; z.B. P({1,2,3}) = {{1,2,3},{1,2},{2,3},{1,3},{1},{2},{3},Ø}

; Funktion "powerset" soll Potenzmenge einer Menge von beliebigen Elementen berechnen
(: powerset ((list-of %a) -> (list-of (list-of %a))))