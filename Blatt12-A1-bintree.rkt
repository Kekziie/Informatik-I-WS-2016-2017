;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt12-A1-bintree) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 1

; Prozedur btree-min
; ermittelt minimalste Markierung eines Binärbaumes
(: btree-min ((btree-of real) -> real))

; Prozedur btree-max
; ermittelt maximalste Markierung eines Binärbaumes
(: btree-max ((btree-of real) -> real))