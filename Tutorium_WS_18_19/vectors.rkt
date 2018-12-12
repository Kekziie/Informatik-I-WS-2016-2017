;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname vectors) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; vektor
; a) Vektoren bestehen aus x-,y- und z-Koordinate
(: make-vector (real real real -> vector))
(: vector? (any -> boolean))
(: vector-x (vector -> real))
(: vector-y (vector -> real))
(: vector-z (vector -> real))

(define-record-procedures vector
   make-vector
   vector?
   (vector-x
    vector-y
    vector-z))

; definieren einiger Punkte
(define v0 (make-vector 0 0 0))
(define v1 (make-vector 0 0 1))
(define v2 (make-vector 0 1 0))
(define v3 (make-vector 1 0 0))
(define v4 (make-vector 1 1 1))
(define v5 (make-vector 0 0 -1))
(define v6 (make-vector 10 0 0))
(define v7 (make-vector 1 2 3))
(define v8 (make-vector -1 -2 -3))
(define v9 (make-vector -10 0 0))
(define v10 (make-vector 0 -1 0))

; b)
; berechnet Diffenrenz zweier Vektoren u und w
(: vec-diff (vector vector -> vector))

(check-expect (vec-diff v7 v7) v0)
(check-expect (vec-diff v0 v1) v5)
(check-expect (vec-diff v7 v8) (make-vector 2 4 6))

(define vec-diff
  (lambda (u w)
    (make-vector (- (vector-x u) (vector-x w))
                 (- (vector-y u) (vector-y w))
                 (- (vector-z u) (vector-z w)))))

; berechnet Summe zweier Vektoren u und w
(: vec-add (vector vector -> vector))

(check-expect (vec-add v0 v0) v0)
(check-expect (vec-add v7 v8) v0)
(check-expect (vec-add v1 v2) (make-vector 0 1 1))

(define vec-add
  (lambda (u w)
    (make-vector (+ (vector-x u) (vector-x w))
                 (+ (vector-y u) (vector-y w))
                 (+ (vector-z u) (vector-z w)))))

; c)
; quadriert eine Zahl x
(: sqr (real -> real))
(check-expect (sqr 1) 1)
(check-expect (sqr 2) 4)
(define sqr
  (lambda (x)
    (* x x)))

; berechnet Länge eines Vektors u
(: vec-length (vector -> real))

(check-expect (vec-length v0) 0)
(check-expect (vec-length v1) 1)
(check-expect (vec-length v2) 1)
(check-within (vec-length v4) (sqrt 3) 0.01)
(check-expect (vec-length v10) 1)

(define vec-length
  (lambda (u)
    (sqrt (+ (sqr (vector-x u))
             (sqr (vector-y u))
             (sqr (vector-z u))))))

;-----------------------------------------------------------
; CUBOID

; d)
; Quader ist definiert durch 2 Vektoren
; erster Vektor VL zeigt auf linke untere vordere Ecke
; zweiter Vektor VR zeigt auf rechte hintere obere Ecke
(: make-cuboid (vector vector -> cuboid))
(: cuboid? (any -> boolean))
(: cuboid-vl (cuboid -> vector))
(: cuboid-vr (cuboid -> vector))

(define-record-procedures cuboid
  make-cuboid
  cuboid?
  (cuboid-vl
   cuboid-vr))

; definieren einige Quader
(define q0 (make-cuboid v0 v0))
(define q1 (make-cuboid v0 v1))
(define q2 (make-cuboid v0 v4))
(define q3 (make-cuboid v1 v4))
(define q4 (make-cuboid v0 v5))
(define q5 (make-cuboid v0 v7))

; e)
; verschiebt einen Quader a entlang eines Vektors u
(: translate-cuboid (cuboid vector -> cuboid))

(check-expect (translate-cuboid q0 v0) q0)
(check-expect (translate-cuboid q1 v1) (make-cuboid v1 (make-vector 0 0 2)))

(define translate-cuboid
  (lambda (a u)
    (make-cuboid (vec-add (cuboid-vl a) u)
                 (vec-add (cuboid-vr a) u))))

; f)
; reflektiert eine Punkt u an einem Punkt w
; Punkte durch Vektroen im Ursprung gegeben
(: point-reflect-point (vector vector -> vector))

(check-expect (point-reflect-point v0 v0) v0)
(check-expect (point-reflect-point v1 v0) v5)

(define point-reflect-point
  (lambda (u w)
    (vec-add w (vec-diff w u))))

; g)
; Hilfsprozeduren
; min aus 2 Vektoren u und w
(: vec-min (vector vector -> vector))
(check-expect (vec-min v0 v1) v0)
(check-expect (vec-min v1 v7) v1)
(define vec-min
  (lambda (u w)
    (make-vector (min (vector-x u) (vector-x w))
                 (min (vector-y u) (vector-y w))
                 (min (vector-z u) (vector-z w)))))

; min aus 2 Vektoren u und w
(: vec-max (vector vector -> vector))
(check-expect (vec-max v0 v1) v1)
(check-expect (vec-max v1 v7) v7)
(define vec-max
  (lambda (u w)
    (make-vector (max (vector-x u) (vector-x w))
                 (max (vector-y u) (vector-y w))
                 (max (vector-z u) (vector-z w)))))

; normalisiert eine Quader
; linkere unter vordere Ecke: min aus Vektoren u und w
; rechte hintere obere Ecke: max aus Vektoren u und w
(: normalized-cuboid (vector vector -> cuboid))

(check-expect (normalized-cuboid v0 v0) q0)
(check-expect (normalized-cuboid v1 v7) (make-cuboid v1 v7))

(define normalized-cuboid
  (lambda (u w)
    (make-cuboid (vec-min u w)
                 (vec-max u w))))

; h)
; spiegelt Quader a an einem Punkt p
(: point-reflect-cuboid (cuboid vector -> cuboid))

(check-expect (point-reflect-cuboid q0 v0) q0)
(check-expect (point-reflect-cuboid q1 v0) q0)

(define point-reflect-cuboid
  (lambda (a p)
    (normalized-cuboid (point-reflect-point (cuboid-vl a) p)
                       (point-reflect-point (cuboid-vl a) p))))

;-----------------------------------------------------------
; SPHERE

; d)
; eine Kugel wird definiert durch eine Vektor m (Position des Mittelpunktes)
; und Radius r
(: make-sphere (vector real -> sphere))
(: sphere? (any -> boolean))
(: sphere-m (sphere -> vector))
(: sphere-r (sphere -> real))

(define-record-procedures sphere
  make-sphere
  sphere?
  (sphere-m
   sphere-r))

; definiere einige Kugeln
(define s0 (make-sphere v0 0))
(define s1 (make-sphere v0 1))
(define s2 (make-sphere v0 3))
(define s3 (make-sphere v1 1))
(define s4 (make-sphere v4 3))
(define s5 (make-sphere v4 1))

; e)
; verschiebe Kugel b entlang eines Vektors u
(: translate-sphere (sphere vector -> sphere))

(check-expect (translate-sphere s0 v0) s0)
(check-expect (translate-sphere s1 v1) s3)
(check-expect (translate-sphere s2 v4) s4)

(define translate-sphere
  (lambda (b u)
    (make-sphere (vec-add (sphere-m b) u)
                 (sphere-r b))))

;f)
; Hilfsprozedur
; addiert einen Wert x auf einen Vektor u
(: vec-addw (vector real -> vector))
(check-expect (vec-addw v0 1) v4)
(check-expect (vec-addw v1 1) (make-vector 1 1 2))
(define vec-addw
  (lambda (u x)
    (make-vector (+ (vector-x u) x)
                 (+ (vector-y u) x)
                 (+ (vector-z u) x))))
; Punkt innerhalb der Kugel?
; Parameter Vector p und Kugel b
(: in-sphere? (vector sphere -> (one-of "inside" "outside" "surface")))

(check-expect (in-sphere? v0 s2) "inside")
(check-expect (in-sphere? (make-vector 42 42 42) s2) "outside")
(check-expect (in-sphere? v1 s1) "surface")

(define in-sphere?
  (lambda (p b)
    (cond ((< (vec-length (vec-diff (sphere-m b) p))
              (sphere-r b)) "inside")
          ((> (vec-length (vec-diff (sphere-m b) p))
              (sphere-r b)) "outside")
          (else "surface"))))

; h)
; spiegelt Kugel b an einen Punkt p
(: point-reflect-sphere (sphere vector -> sphere))

(check-expect (point-reflect-sphere s3 v0) (make-sphere v5 1))

(define point-reflect-sphere
  (lambda (b p)
    (make-sphere (point-reflect-point (sphere-m b) p)
                 (sphere-r b))))
    