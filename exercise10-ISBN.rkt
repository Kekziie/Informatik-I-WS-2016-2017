;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise10-ISBN) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm") (lib "starwars.rkt" "installed-teachpacks"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm") (lib "starwars.rkt" "installed-teachpacks")))))
; Übung 10 ISBN Check

; Implementieren einer Prozedur die eine 13-stellige Zahl überprüft
; Verfahren:
; 1.) Summe aus dreifachen Wert der Ziffern an geraden Stellen
; 2.) Summe aus Wert der Ziffern an ungeraden Stellen
; 3.) Summe aus 1.) und 2.)
; 4.) Division durch 10
; 5.) wenn Rest 0 -> ISBN #t