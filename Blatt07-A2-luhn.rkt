;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt07-A2-luhn) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 2 Luhn-Check
; Verfahren definiert für einer Zahl n
; i) gehe die Ziffern von n, rechts nach links durch, verdopple jede 2.Ziffer
; ii) falls Verdopplung 2 Ziffern hat -> Aufspaltung in 2 unabhängig voneinander zu betrachtenden Ziffern
; iii) Summe aus allen Ziffern
; iv) dividiere durch 10
; v) Rest der Division 0 -> gültige Zahl n, Luhn-Check bestanden
;    Rest ungleich 0 -> ungültige Zahl n, Luhn-Check nicht bestanden

; (a)
; Prozedur "sum" soll Summe einer Liste von Zahlen berechnen

(: sum ((list-of number) -> number))

(check-expect (sum empty-list) empty)
(check-expect (sum (list 1 2 3)) 6)
(check-expect (sum (list 0)) 0)
(check-expect (sum (list 2 -2 3)) 3)
(check-within (sum (list 1.5 2.5 -5 0)) -1 0.01)
(check-within (sum (list 2 1/2 -1)) 1.5)
