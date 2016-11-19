;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt05-A2-balance) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 2

; vergleichen von Flächen unterschiedlicher geometrischer Formen (shapes) mit Hilfe einer "Waage"
; dabei soll der Flächeninhalt als "Gewicht" dienen

; (a) Recorddefinitionen für 3 geometrische Formen mit ihrer Beschreibung
;      - Kreis: Radius Modus Farbe
;      - Rechteck: Breite Höhe Modus Farbe
;      - Dreieck (gleichseitig): Seitenlänge Modus Farbe

(define modus "solid")

(define mode
  (signature (predicate modus)))

(: make-circle (real mode string -> circle))

(: circle-radius (circle -> real))
(: circle-mode (circle -> mode))
(: circle-color (circle -> color))

(define-record-procedures circle
  make-circle
  circle?
  (circle-radius
   circle-mode
   circle-color))

(: make-rectangle (real real mode string -> rectangle))

(: rectangle-width (rectangle -> real))
(: rectangle-length (rectangle -> real))
(: rectangle-mode (rectangle -> mode))
(: rectangle-color (rectangle -> string))

(define-record-procedures rectangle
  make-rectangle
  rectangle?
  (rectangle-width
   rectangle-length
   rectangle-mode
   rectangle-color))

(: make-triangle (real mode string -> triangle))

(: triangle-side-length (triangle -> real))
(: triangle-mode (triangle -> mode))
(: triangle-color (triangle -> string))

(define-record-procedures triangle
  make-triangle
  triangle?
  (triangle-side-length
   triangle-mode
   triangle-color))