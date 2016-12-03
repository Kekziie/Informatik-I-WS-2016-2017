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

; definieren von Datendefinitionen
; für Testfälle später

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
       (/ (* (triangle-side-length x) (triangle-side-length x))
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
; der Abstand w2 soll 40 pixel breit sein, d.h. Lücke = 40px

; zeichnen der Waage ohne Formen

(define scale-color "black")
(define scale-height 10)

(: scale-form (shape shape -> image))

(check-expect (scale-form Kreis1 Rechteck1) (rectangle 290 scale-height "solid" scale-color))
(check-within (scale-form Dreieck2 Rechteck3) (rectangle 51.3 scale-height "solid" scale-color) 0.01)

(define scale-form
  (lambda (Form1 Form2)
    (rectangle (scale-length Form1 Form2 40) scale-height "solid" scale-color)))

; zeichnen der Waage mit Formen

(: draw-scale1 (shape shape -> image))

(define draw-scale1
  (lambda (form1 form2)
    (above (beside/align "bottom"
                         (draw-shape form1)
                         (empty-scene 40 0)
                         (draw-shape form2))
           (scale-form form1 form2))))

; zeichnet Dreieck und Rechteck auf einer "waagrechten" Waage
;(draw-scale1 Dreieck1 Rechteck1)

; (g) Funktionalität der Waage

; Rotationswinkel x berechnet aus Flächeninhalt A1, A2 von Formen
; A1 und A2 ungleich 0
; wenn A1>A2: x=90*(1- A2/A1)
; sonst: x=90*(-1+A1/A2)

(: rotation-angle (real real -> real))

(check-expect (rotation-angle 2 1) 45)
(check-within (rotation-angle 10.5 4.5) 51.4286 0.01)
(check-expect (rotation-angle 1 2) -45)
(check-within (rotation-angle 2.5 10.5) -68.5714 0.01)

(define rotation-angle
  (lambda (A1 A2)
    (if (> A1 A2)
        (* 90
           (- 1
              (/ A2 A1)))
        (* 90
           (+ -1
              (/ A1 A2))))))

; zeichnen der Waage mit Funktionalität

(: draw-scale (shape shape -> image))

(define draw-scale
  (lambda (form1 form2)
    (if (< (shape-area form1) (shape-area form2))
        (rotate (rotation-angle (shape-area form1) (shape-area form2))
                (draw-scale1 form1 form2))
        (rotate (- (rotation-angle (shape-area form1) (shape-area form2)))
                (draw-scale form1 form2)))))

; Flächeninhalt Dreieck1 2435.69 < Rechteck1 2500
; minimaler Unterschiede, d.h. Waage nur leicht Richtung Rechteck geneigt
(draw-scale Dreieck1 Rechteck1)

; Flächeninhalt Kreis1 628.3 < Dreieck1 2435.69
; Waage deutlich schwerer auf Seite des Dreiecks
(draw-scale Kreis1 Dreieck1)