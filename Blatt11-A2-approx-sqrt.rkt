;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt11-A2-approx-sqrt) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 2
; Definitionen
; (promise t): Versprechen, einen Wert der Signatur t zu liefern
(define promise
  (lambda (t)
    (signature (-> t))))

; erzwungene Auswertung des Promise p
(: force ((promise %a) -> %a))
(define force
  (lambda (p) (p)))

; polymorphes Paar
(: make-cons (%a %b -> (cons-of %a %b)))
(: head ((cons-of %a %b) -> %a))
(: tail ((cons-of %a %b) -> %b))
(define-record-procedures-parametric cons cons-of
  make-cons
  cons?
  (head
   tail))

; ein Stream besteht aus: erstes Element (head) und einem Promise, der den Rest des Streams generieren kann (tail)
(define stream-of
  (lambda (t)
    (signature (cons-of t
                        (promise (stream-of t))))))

; "stream-take" nimmt erste n Elemente des Streams str in eine Liste extrahieren
(: stream-take (natural (stream-of %a) -> (list-of %a)))
(define stream-take
  (lambda (n str)
    (if (= n 0)
        empty
        (make-pair (head str)
                   (stream-take (- n 1) (force (tail str)))))))

; Stream der Zahlen ab n (n, n+1, n+2, ...)
(: from (number -> (stream-of number)))
(check-expect (stream-take 5 (from 1)) (list 1 2 3 4 5))
(check-expect (stream-take 3 (from 0)) (list 0 1 2))
(check-expect (stream-take 10 (from -10)) (list -10 -9 -8 -7 -6 -5 -4 -3 -2 -1))
(define from
  (lambda (n)
    (make-cons n
               (lambda () (from (+ n 1))))))

; (a)
; (stream-iterate f x) erzeugt einen Strom
; beginnt mit x und weitere Elemente, sind die Ergebnisse der wiederholten Anwendung von f: x, f(x), f(f(x)), f(f(f(x))), ...
(: stream-iterate ((%a -> %a) %a -> (stream-of %a)))

(check-expect (stream-take 5 (stream-iterate (lambda (x) (+ x 2)) 1)) (list 1 3 5 7 9))
(check-expect (stream-take 5 (stream-iterate (lambda (x) (- x 0)) 1)) (list 1 1 1 1 1))
(check-expect (stream-take 1 (stream-iterate (lambda (x) (/ x 2)) 1)) (list 1))
(check-expect (stream-take 3 (stream-iterate (lambda (x) (* x 2)) 1)) (list 1 2 4))

(define stream-iterate
  (lambda (f x)
    (make-cons x
               (lambda () (stream-iterate f (f x))))))

; (b)
; (stream-converge d s) liefert erstes Element aus einem konvergierenden Strom s
; unterscheidet sich um weniger als d von seinem Vorgänger
(: stream-converge (real (stream-of real) -> real))

(check-within (stream-converge 0.3 (stream-iterate (lambda (x) (/ x 10)) 100)) 0.01 0.00001)
(check-expect (stream-converge 0.5 (stream-iterate (lambda (x) (* x 2))) 1) 2)
(check-within (stream-converge 0.1 (stream-iterate (lambda  (x) (/ x 2)) 1)) 0.06 0.0001)
(check-expect (stream-converge 2 (stream-iterate (lambda (x) (+ x 2)) 2) 0))


 
; (c)
; (approx-sqrt a delta) berechnet Näherungswert (Wurzel a)
; vorhergehender Approximationswert soll sich um weniger als delta unterscheiden
;(: approx-sqrt (real real -> real))

;(check-within (approx-sqrt 15 0.01) 3.872 0.001)