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
; 