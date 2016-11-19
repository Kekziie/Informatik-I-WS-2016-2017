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

; definieren von Testformen

(define Kreis1
  (make-circle 100 "green"))

(define Kreis2
  (make-circle 5.5 "Khaki"))

(define Kreis3
  (make-circle 1.5 "Crimson"))

(define Rechteck1
  (make-rectangle 50 50 "chocolate"))

(define Rechteck2
  (make-rectangle 2.5 20 "Coral"))

(define Rechteck3
  (make-rectangle 8 5.5 "Aquamarine"))

(define Dreieck1
  (make-triangle 75 "Orchid"))

(define Dreieck2
  (make-triangle 3.3 "MistyRose"))

(define Dreieck3
  (make-triangle 11 "RoyalBlue"))

; (b) definieren einer Signatur "shape", die alle Formen umfasst

(define shape
  (signature (mixed rectangles circles triangles)))

; (c) Prozedur "shape-area" soll für jede beliebige Fläche Flächeninhalt berechnen
; Prozeduren die Flächeninhalt ausrechnen

(define pi 3.1415)

(: circle-area (circles -> real))

(check-within (circle-area Kreis1) 628.30 0.01)
(check-within (circle-area Kreis2) 34.5565 0.01)

(define circle-area
  (lambda (x)
    (* 2 pi (circle-radius x))))

(: rectangle-area (rectangles -> real))

(check-expect (rectangle-area Rechteck1) 2500)
(check-within (rectangle-area Rechteck2) 50.0 0.01)

(define rectangle-area
  (lambda (x)
    (* (rectangle-width x)
       (rectangle-height x))))

(: triangle-area (triangles -> real))

(check-within (triangle-area Dreieck1) 2435.69 0.01)
(check-within (triangle-area Dreieck2) 4.7155 0.01)

(define triangle-area
  (lambda (x)
    (* (sqrt 3)
       (/ (expt (triangle-side-length x) 2)
          4))))

(: shape-area (shape -> real))

(check-within (shape-area Kreis1) 628.30 0.01)
(check-expect (shape-area Rechteck1) 2500)
(check-within (shape-area Dreieck1) 2435.69 0.01)

(define shape-area
  (lambda (x)
    (cond
      ((circle? x) (circle-area x))
      ((rectangle? x) (rectangle-area x))
      ((triangle? x) (triangle-area x)))))

; (d) für "Zeichnen" einer Waage benötigt man: Länge des Balkens der Waage
;     Brechnung aus w1 und w3: Breiten der Bilder und w2 als Abstand zwischen w1 und w3

(: width1 (shape -> real))

(check-expect (width1 Kreis1) 200)
(check-expect (width1 Rechteck1) 50)
(check-expect (width1 Dreieck1) 75)

(define width1
  (lambda (x)
    (cond
      ((circle? x) (* 2 (circle-radius x)))
      ((rectangle? x) (rectangle-width x))
      ((triangle? x) (triangle-side-length x)))))

(define w2 40)

(: scale-length (shape shape real -> real))

(check-expect (scale-length Kreis1 Rechteck1 40) 290)
(check-expect (scale-length Dreieck1 Kreis1 50) 325)
(check-expect (scale-length Rechteck1 Dreieck1 0) 125)

(define scale-length
  (lambda (w1 w3 w2)
    (+ (width1 w1) (width1 w3) w2)))

; (e) Prozedur die jeweilige Form zeichnet

(: draw-shape (shape -> image))

(check-expect (draw-shape Kreis3) (circle 1.5 "solid" "Crimson"))
(check-expect (draw-shape Rechteck3) (rectangle 8 5.5 "solid" "Aquamarine"))
(check-expect (draw-shape Dreieck3) (triangle 11 "solid" "RoyalBlue"))

(define draw-shape
  (lambda (x)
    (cond
      ((circle? x)
       (circle (circle-radius x) "solid" (circle-color x)))
      ((rectangle? x)
       (rectangle (rectangle-width x) (rectangle-height x) "solid" (rectangle-color x)))
      ((triangle? x)
       (triangle (triangle-side-length x) "solid" (triangle-color x))))))

; (f) "zeichnen" der Waage



;(: draw-scale (shape shape -> image))


