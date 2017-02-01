;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise5-vectors) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Übung 5 Vektoren

; 1) Record- und Datendefinition

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

; 2) Vektoraddition
(: vec-add (vector vector -> vector))

(check-expect (vec-add Vec1 Vec3) Vec3)
(check-expect (vec-add Vec2 Vec3) (make-vector 1   1   2))
(check-expect (vec-add Vec3 Vec4) (make-vector 0   1   2))
(check-within (vec-add Vec6 Vec3) (make-vector 1/2 1.5 1)   0.001)
(check-within (vec-add Vec5 Vec6) (make-vector 1/2 1.5 2.5) 0.001)

(define vec-add
  (lambda (vector1 vector2)
    (make-vector (+ (vector-x vector1) (vector-x vector2))
                 (+ (vector-y vector1) (vector-y vector2))
                 (+ (vector-z vector1) (vector-z vector2)))))

; 3) Vektorsubtraktion
(: vec-sub (vector vector -> vector))

(check-expect (vec-sub Vec1 Vec2) (make-vector  0      0  -1))
(check-expect (vec-sub Vec3 Vec2) (make-vector  1      1   0))
(check-expect (vec-sub Vec4 Vec3) (make-vector -2     -1   0))
(check-within (vec-sub Vec6 Vec3) (make-vector -1.5 -1/2  -1) 0.001)
(check-within (vec-sub Vec5 Vec6) (make-vector  1.5  1/2 2.5) 0.001)

(define vec-sub
  (lambda (vector1 vector2)
    (make-vector (- (vector-x vector1) (vector-x vector2))
                 (- (vector-y vector1) (vector-y vector2))
                 (- (vector-z vector1) (vector-z vector2)))))

; 4) Betrag (absolute value) eines Vektors berechnen
(: absolute-value (vector -> real))

(check-expect (absolute-value Vec1) 0)
(check-expect (absolute-value Vec2) 1)
(check-within (absolute-value Vec3) (sqrt 3)    0.001)
(check-within (absolute-value Vec4) (sqrt 2)    0.001)
(check-within (absolute-value Vec5) (sqrt 8.25) 0.001)
(check-within (absolute-value Vec6) (sqrt 1/2)  0.001)

(define absolute-value
  (lambda (vector)
    (sqrt (+ (expt (vector-x vector) 2)
             (expt (vector-y vector) 2)
             (expt (vector-z vector) 2)))))

; 5) Multiplikation eines Vektors mit einer reelen Zahl c
(: vec-mul (real vector -> vector))

(check-expect (vec-mul 10 Vec3) (make-vector   10   10   10))
(check-expect (vec-mul 25 Vec4) (make-vector  -25    0   25))
(check-within (vec-mul 5 Vec5) (make-vector     5    5 12.5) 0.001)
(check-within (vec-mul -5 Vec6) (make-vector  2.5 -2.5    0) 0.001)
(check-within (vec-mul -2.5 Vec2) (make-vector  0    0 -2.5) 0.001)

(define vec-mul
  (lambda (c Vector)
    (make-vector (* c (vector-x vector))
                 (* c (vector-y vector))
                 (* c (vector-z vector)))))

; 6) Mittelpunkt einer Strecke zwischen zwei Vektoren
(: mid-2-vec (vector vector -> vector))

(check-within (mid-2-vec Vec1 Vec2) (make-vector 0 0 0.5) 0.001)

(define mid-2-vec
  (lambda (vector1 vector2)
    (vec-mul 1/2 (vec-add vector1 vector2))))



  
  