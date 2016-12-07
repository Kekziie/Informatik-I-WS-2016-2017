;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname exercise6-shapes) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Übung 6
; a) Formen 2D

; Record- und Datendefinition

; ein Rechteck besteht aus
; - Breite
; - Höhe
; - Farbe
(: make-my-rectangle (real real string -> my-rectangle))
(: my-rectangle-width (my-rectangle -> real))
(: my-rectangle-heigth (my-rectangle -> real))
(: my-rectangle-color (my-rectangle -> string))
(: my-rectangle? (any -> boolean))

(define-record-procedures my-rectangle
  make-my-rechteck
  my-rectangle?
  (my-rectangle-width
   my-rectangle-heigth
   my-rectangle-color))

; ein Kreis besteht aus
; - Radius
; - Farbe
(: make-my-circle (real string -> my-circle))
(: my-circle-radius (my-circle -> real))
(: my-circle-color (my-circle -> string))
(: my-circle? (any -> boolean))

(define-record-procedures my-circle
  make-my-circle
  my-circle?
  (my-circle-radius
   my-circle-color))

; ein gleichseitiges Dreieck besteht aus
; - Seitenlänge
; - Farbe
(: make-my-triangle (real string -> my-triangle))
(: my-triangle-length (my-triangle -> real))
(: my-triangle-color (my-triangle -> string))
(: my-triangle? (any -> boolean))

(define-record-procedures my-triangle
  make-my-triangle
  my-triangle?
  (my-triangle-length
   my-triangle-color))

; Datendefinition
(define rectangle1
  (make-my-rectangle 20 10 "orchid"))

(define rectangle2
  (make-my-rectangle 2.5 1.5 "aqua"))

(define rectangle3
  (make-my-rectangle 50 50 "gold"))

(define circle1
  (make-my-circle 50 "maroon"))

(define circle2
  (make-my-circle 25 "olive"))

(define circle3
  (make-my-circle 2.5 "MistyRose"))

(define triangle1
  (make-my-triangle 25 "beige"))

(define triangle2
  (make-my-triangle 10 "lime"))

(define triangle3
  (make-my-triangle 2.5 "indigo"))
