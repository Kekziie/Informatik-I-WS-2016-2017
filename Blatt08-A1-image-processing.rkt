;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt08-A1-image-processing) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 8
; arbeiten mit Bildern:
; - vertikal spiegeln
; - horizontal spiegeln
; - 90° gegen Uhrzeigersinn rotieren

; definieren eines Testbilder
(define Testbild
  (above (beside (square 10 "solid" "orchid")
                 (square 10 "solid" "aqua"))
         (beside (square 10 "solid" "gold")
                 (square 10 "solid" "magenta"))))

; Prozedur "rows-worker" soll
; - eine natürliche Zahl w und eine Liste xs akzeptieren
; - übergebene Liste xs soll in eine Liste von Listen xss umgewandelt werden
; - jede innere Liste soll w Elemnte enthalten
; - letzte inner Lite kann weniger als w Elemente enthalten

; Prozedur "rows" soll auf "rows-worker" zugreifen -> endrekursiv

; schreiben einer Prozedur "flatten", die
; - eine Liste von Listen xss akzeptiert
; - eine Liste von Listen in eine Liste wandelt, die alle Elemente aus xss  enthält

; schreiben einer Prozedur "transpose", die
; - eine Liste von Listen xss akzeptiert
; - die übergebene Liste "transponiert"
;   (bei 2-dimensionalen Matrix: Zeilen zu Spalten umwandelt)
; - innere Listen jeweils gleich lang

;================================================================================================

; Zugriff auf die Liste der Bildpunkte (Pixel) eines Bildes:  
; (: image->color-list (image -> (list-of rgb-color)))  
; (: color-list->bitmap ((list-of rgb-color) natural natural -> image))

; Prozedur "vert-mirror" soll Bild vertikal spiegeln

; Prozedur "horiz-mirror" soll Bild horizontal spiegeln

; Prozedur "rotate-90-left" soll ein Bild um 90° gegen Uhrzeigersinn rotieren