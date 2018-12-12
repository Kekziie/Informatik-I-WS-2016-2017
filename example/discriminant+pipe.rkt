;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname discriminant+pipe) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; berechnet Diskriminante D=b^2-4*ac
(: discriminant (real real real -> real))

(check-expect (discriminant 0 0 0) 0)
(check-within (discriminant -1 1 1) 5 0.001)

(define discriminant
  (lambda (a b c)
    (- (expt b 2)
       (* 4 a c))))

; Mitternachtsformel berechnet x1 mit Additon
(: mitternacht-1 (real real real -> real))

(check-expect (mitternacht-1 1 0 0) 0)
(check-within (mitternacht-1 1 -2 1) 1 0.001)

(define mitternacht-1
  (lambda (a b c)
    (/ (+ (- b) (sqrt (discriminant a b c)))
       (* 2 a))))


; Mitternachtsformel berechnet x2 mit Subtraktion
(: mitternacht-2 (real real real -> real))

(check-expect (mitternacht-2 1 0 0) 0)
(check-within (mitternacht-1 -1 -2 -1) -1 0.001)

(define mitternacht-2
  (lambda (a b c)
    (/ (- (- b) (sqrt (discriminant a b c)))
       (* 2 a))))

; Konstante Pi
(: Pi real)
(define Pi 3.1415)

; Flächeninhalt eines Kreises mit Radius r
(: circle-area (real -> real))

(check-expect (circle-area 0) 0)
(check-within (circle-area 1) 3.1415 0.001)

(define circle-area
  (lambda (r)
    (* Pi (expt r 2))))

; Umfang eines Kreise mir Radius r
(: circle-perimeter (real -> real))

(check-expect (circle-perimeter 0) 0)
(check-within (circle-perimeter 1) 6.283 0.001)

(define circle-perimeter
  (lambda (r)
    (* 2 Pi r)))

; Mantelfläche eines Zylinders mit Radius r und Höhe h
(: cylinder-side-area (real real -> real))

(check-expect (cylinder-side-area 0 0) 0)
(check-within (cylinder-side-area 1 1) 6.283 0.001)

(define cylinder-side-area
  (lambda (r h)
    (* (circle-perimeter r) h)))

; Oberfläche eines Zylinders mit Radius r und Höhe h
(: cylinder-area (real real -> real))

(check-expect (cylinder-area 0 0) 0)
(check-within (cylinder-area 1 1) 12.566 0.001)

(define cylinder-area
  (lambda (r h)
    (+ (* (circle-area r) 2)
       (cylinder-side-area r h))))

; Oberfläche eines Hohlzylinders mit äußerer Radius r1 und innerer Radius r2 und der Höhe h
(: pipe-area (real real real -> real))

(check-expect (pipe-area 0 0 0) 0)
(check-within (pipe-area 2 1 1) 43.981 0.001)

(define pipe-area
  (lambda (r1 r2 h)
    (- (+ (cylinder-area r1 h)
          (cylinder-area r2 h))
       (* 2 (circle-area r2)))))
 
