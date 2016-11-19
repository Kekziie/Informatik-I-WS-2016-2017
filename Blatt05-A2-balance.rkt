;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt05-A2-balance) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 2

; vergleichen von Flächen unterschiedlicher geometrischer Formen (shapes) mit Hilfe einer "Waage"
; dabei soll der Flächeninhalt als "Gewicht" dienen

; (a) Recorddefinitionen für 3 geometrische Formen mit ihrer Beschreibung
;      - Kreis: Radius r, Farbe c
;      - Rechteck: Breite w, Höhe h, Farbe c
;      - Dreieck (gleichseitig): Seitenlänge s, Farbe c

(: make-circle (real string -> circles))

(: circle-radius (circles -> real))
(: circle-color (circles -> string))

(check-property
 (for-all ((r real)
           (c string))
   (and (= (circle-radius (make-circle r c)) r)
        (string=? (circle-color (make-circle r c)) c))))

(define-record-procedures circles
  make-circle
  circle?
  (circle-radius
   circle-color))

(: make-rectangle (real real string -> rectangles))

(: rectangle-width (rectangles -> real))
(: rectangle-height (rectangles -> real))
(: rectangle-color (rectangles -> string))

(check-property
 (for-all ((w real)
           (h real)
           (c string))
   (and (= (rectangle-width (make-rectangle w h c)) w)
        (= (rectangle-height (make-rectangle w h c)) h)
        (string=? (rectangle-color (make-rectangle w h c)) c))))

(define-record-procedures rectangles
  make-rectangle
  rectangle?
  (rectangle-width
   rectangle-height
   rectangle-color))

(: make-triangle (real string -> triangles))

(: triangle-side-length (triangles -> real))
(: triangle-color (triangles -> string))

(check-property
 (for-all ((s real)
           (m string)
           (c string))
   (and (= (triangle-side-length (make-triangle s c)) s)
        (string=? (triangle-color (make-triangle s c)) c))))

(define-record-procedures triangles
  make-triangle
  triangle?
  (triangle-side-length
   triangle-color))
