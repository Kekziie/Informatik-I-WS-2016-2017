;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt05-A2-balance) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 2

; vergleichen von Flächen unterschiedlicher geometrischer Formen (shapes) mit Hilfe einer "Waage"
; dabei soll der Flächeninhalt als "Gewicht" dienen

; (a) Recorddefinitionen für 3 geometrische Formen mit ihrer Beschreibung
;      - Kreis: Radius r, Modus m, Farbe c
;      - Rechteck: Breite w, Höhe h, Modus m, Farbe c
;      - Dreieck (gleichseitig): Seitenlänge s, Modus m, Farbe c

(: make-circle (real string string -> circle))

(: circle-radius (circle -> real))
(: circle-mode (circle -> string))
(: circle-color (circle -> string))

(check-property
 (for-all ((r real)
           (m string)
           (c string))
   (and (= (circle-radius (make-circle r m c)) r)
        (string=? (circle-mode (make-circle r m c)) m)
        (string=? (circle-color (make-circle r m c)) c))))

(define-record-procedures circle
  make-circle
  circle?
  (circle-radius
   circle-mode
   circle-color))

(: make-rectangle (real real string string -> rectangle))

(: rectangle-width (rectangle -> real))
(: rectangle-height (rectangle -> real))
(: rectangle-mode (rectangle -> string))
(: rectangle-color (rectangle -> string))

(define-record-procedures rectangle
  make-rectangle
  rectangle?
  (rectangle-width
   rectangle-height
   rectangle-mode
   rectangle-color))

(: make-triangle (real string string -> triangle))

(: triangle-side-length (triangle -> real))
(: triangle-mode (triangle -> string))
(: triangle-color (triangle -> string))

(define-record-procedures triangle
  make-triangle
  triangle?
  (triangle-side-length
   triangle-mode
   triangle-color))



