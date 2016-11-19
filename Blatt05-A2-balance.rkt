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

(check-property
 (for-all ((w real)
           (h real)
           (m string)
           (c string))
   (and (= (rectangle-width (make-rectangle w h m c)) w)
        (= (rectangle-height (make-rectangle w h m c)) h)
        (string=? (rectangle-mode (make-rectangle w h m c)) m)
        (string=? (rectangle-color (make-rectangle w h m c)) c))))

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

(check-property
 (for-all ((s real)
           (m string)
           (c string))
   (and (= (triangle-side-length (make-triangle s m c)) s)
        (string=? (triangle-mode (make-triangle s m c)) m)
        (string=? (triangle-color (make-triangle s m c)) c))))

(define-record-procedures triangle
  make-triangle
  triangle?
  (triangle-side-length
   triangle-mode
   triangle-color))

; definieren von Testformen

(define Kreis1
  (make-circle 100 "solid" "green"))

(define Kreis2
  (make-circle 5.5 "sold" "Khaki"))

(define Rechteck1
  (make-rectangle 50 50 "solid" "chocolate"))

(define Rechteck2
  (make-rectangle 2.5 20 "solid" "Coral"))

(define Dreieck1
  (make-triangle 75 "solid" "Orchid"))

(define Dreieck2
  (make-triangle 3.3 "solid" "MistyRose"))

; (b) definieren einer Signatur "shape", die alle Formen umfasst

(define shape
  (signature (mixed rectangle circle triangle)))

; (c) Prozedur "shape-area" soll für jede beliebige Fläche Flächeninhalt berechnen

; Prozeduren die Flächeninhalt ausrechnen

(define pi 3.1415)

(: circle-area (circle -> real))

(check-within (circle-area Kreis1) 628.30 0.01)
(check-within (circle-area Kreis2) 34.5565 0.01)

(define circle-area
  (lambda (x)
    (* 2 pi (circle-radius x))))

(: rectangle-area (rectangle -> real))

(check-expect (rectangle-area Rechteck1) 2500)
(check-within (rectangle-area Rechteck2) 50.0 0.01)

(define rectangle-area
  (lambda (x)
    (* (rectangle-width x)
       (rectangle-height x))))

(: triangle-area (triangle -> real))

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

(: scale-length (image image real -> real))

(define w1
  (lambda (x)
    (cond
      ((circle? x) (* 2 (circle-radius x)))
      ((rectangle? x) (rectangle-width x))
      ((triangle? x) (triangle-side-length x)))))
    