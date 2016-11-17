;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname exercise4-clock) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; animieren sie ein Uhr mithilfe der Funktion von "pinhole"

; definieren der Größe, des Modus und der Farbe

(define second-hand-width 6)
(define second-hand-length 90)
(define minute-hand-width 10)
(define minute-hand-length 80)
(define hour-hand-width 16)
(define hour-hand-length 70)
(define hour-mark-width 10)
(define hour-mark-length 20)
(define clock-radius 100)

(define mode "solid")

(define second-hand-color "violet red")
(define minute-hand-color "orchid")
(define hour-hand-color "lightpink")

; "malen" der Uhr
(define second-hand (put-pinhole 3 85 (rectangle second-hand-width second-hand-length mode "lightblue"))) ; Sekundenzeiger
(define minute-hand (put-pinhole 5 75 (rectangle minute-hand-width minute-hand-length mode "blue"))) ; Minutenzeiger
(define hour-hand (put-pinhole 8 65 (rectangle hour-hand-width hour-hand-length mode "red"))) ; Stundenzeiger
(define hm (put-pinhole 5 clock-radius (rectangle hour-mark-width hour-mark-length mode "white"))) ;  "Stundenmarkierungen" (hour-mark hm)
; Hintergrund der Uhr mit "Stundenmarkierungen"
(define hintergrund (overlay/pinhole hm
                                     (rotate 30 hm)
                                     (rotate 60 hm)
                                     (rotate 90 hm)
                                     (rotate 120 hm)
                                     (rotate 150 hm)
                                     (rotate 180 hm)
                                     (rotate 210 hm)
                                     (rotate 240 hm)
                                     (rotate 270 hm)
                                     (rotate 300 hm)
                                     (rotate 330 hm)
                                     (circle clock-radius mode "black")))
