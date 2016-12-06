;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise5-vectors) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Übung 5 Vektoren

; Record- und Datendefinition

; ein Vektor (vector) besteht aus
; - x-Koordinate
; - y-Koordinate
; - z-Koordinate

(: make-vector (real real real -> vector))
(: vector-x (vector -> real))
(: vector-y (vector -> real))
(: vector-z (vector -> real))
(: vector? (any -> boolean))

(define-record-procedures vector
  make-vector
  vector?
  (vector-x
   vector-y
   vector-z))

; Datendefinitionen

(define Vec1
  (make-vector 0 0 0))

(define Vec2
  (make-vector 0 0 1))

(define Vec3
  (make-vector 1 1 1))

(define Vec4
  (make-vector -1 0 1))

(define Vec5
  (make-vector 1 1 2.5))

(define Vec6
  (make-vector -1/2 1/2 0))
  
  