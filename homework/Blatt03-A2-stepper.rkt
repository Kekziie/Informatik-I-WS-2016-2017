;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt03-A2-stepper) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 2
; angeben der Reduktionsregeln folgender Ausdrücke im Stepper

((lambda (a) a)
 (+ ((lambda (a) (+ a 2)) 3) 2))


; 1) apply(λ)
;    ((lambda (a) a)
;     (+ (+ 3 2) 2))

; 2a) eval(id)
;     ((lambda (a) a)
;      (+ (#<procedure:+> 3 2) 2))


; 2b) apply(prim (+))
;     ((lambda (a) a)
;      (+ 5 2))

; 3a) eval(id)
;     ((lambda (a) a)
;      (#<procedure:+> 5 2))

; 3b) apply(prim (+))
;    ((lambda (a) a) 7)

; 4) apply(λ)
;    7

(define pi 3.141)
(* 2 pi)

; 1) eval(id)
;    (define pi 3.141)
;    (* 2 3.141)

; 2a) eval(id)
;    (define pi 3.141)
;    (#<procedure:*> 2 3.141)

; 2b) apply(prim (*))
;     (define pi 3.141)
;     6.282

((lambda (pi) (* 2 pi)) pi)

; 1) eval(id)
;    (define pi 3.141)
;    ((lambda (pi) (* 2 pi)) 3.141)

; 2) apply(λ)
;    (define pi 3.141)
;    (* 2 3.141)

; 3a) eval(id)
;     (#<procedure:*> 2 3.141)

; 3b) apply(prim (*))
;     6.282

(define quadrat
  (lambda (n)
    (* n n)))
(quadrat (+ 4 2))

; 1) eval(id)
;    (define quadrat (lambda (n) (* n n)))
;    ((lambda (n) (* n n)) (+ 4 2))

; 2a) eval(id)
;     ((lambda (n) (* n n)) (#<procedure:+> 4 2))

; 2b) apply(prim (+))
;     ((lambda (n) (* n n)) 6)

; 3) apply(λ)
;    (* 6 6)

; 4a) eval(id)
;     (#<procedure:*> 6 6)

; 4b) apply(prim (*))
;     36

((lambda (x) x)
 (lambda (x) x))

; 1) apply(λ)
;   (lambda (x) x)