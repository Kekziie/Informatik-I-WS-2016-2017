;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise9-streams) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm") (lib "starwars.rkt" "installed-teachpacks"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm") (lib "starwars.rkt" "installed-teachpacks")))))
; Übung 9 Streams

; Definition
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

; Prozedur "stream-map"
; - akzeptiert Prozedur f und Strom str
; - liefert Strom zurück, nach Anwedung f auf Elemente vom Strom
(: stream-map ((%a -> %b) (stream-of %a) -> (stream-of %b)))
(check-expect (stream-take 3 (stream-map (lambda (n) (+ n 2)) (from 1))) (list 3 4 5))
(check-expect (stream-take 5 (stream-map (lambda (n) (* n 2)) (from 0))) (list 0 2 4 6 8))
(define stream-map
  (lambda (f str)
    (make-cons (f (head str))
               (lambda () (stream-map f (force (tail str)))))))

; Prozedur "last" liefert letztes Element der Liste
(: last ((list-of %a) -> %a))
(check-expect (last empty) empty)
(check-expect (last (list 1)) 1)
(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list "a" "b")) "b")
(define last
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((empty? (rest xs)) (first xs))
      (else (last (rest xs))))))

; 1.)
; Prozedur stream-drop
; - akzeptiert eine natürliche Zahl n und einen Stream str
; - erzeugt einen neuen Stream
; - verwirft die n ersten Elemente aus geg. Stream
(: stream-drop (natural (stream-of %a) -> (stream-of %a)))

(check-expect (stream-take 5 (stream-drop 5 (from 0))) (list 5 6 7 8 9))
(check-expect (stream-take 10 (stream-drop 1 (from 0))) (list 1 2 3 4 5 6 7 8 9 10))
(check-expect (stream-take 3 (stream-drop 2 (from 3))) (list 5 6 7))
(check-expect (stream-take 5 (stream-drop 0 (from 1))) (list 1 2 3 4 5))

(define stream-drop
  (lambda (n str)
    (if (= n 0)
        str
        (stream-drop (- n 1) (force (tail str))))))

; 2.)
; Prozedur stream-alternating
; - akzeptiert zwei Elemente a und b mit Signatur %a
; - erstellt einen alternierenden Stream von a und b
(: stream-alternating (%a %a -> (stream-of %a)))

(check-expect (stream-take 5 (stream-alternating "a" "b")) (list "a" "b" "a" "b" "a"))
(check-expect (stream-take 10 (stream-alternating 1 2)) (list 1 2 1 2 1 2 1 2 1 2))
(check-expect (stream-take 3 (stream-alternating #t #f)) (list #t #f #t))

(define stream-alternating
  (lambda (a b)
    (make-cons a
               (lambda () (stream-alternating b a)))))

; 3.)
; Prozedur stream-cycle-worker
; - akzeptiert eine Liste und einen Akku
; - erzeugt einen Stream, der die Elemente der Liste wiederholt
; - bei leerer Liste wird die Liste mit dem Akku "aufgefüllt"
(: stream-cycle-worker ((list-of %a) (list-of %a) -> (stream-of %a)))
(define stream-cycle-worker
  (lambda (xs acc)
    (if (empty? xs)
        (stream-cycle-worker acc acc)
        (make-cons (first xs)
                   (lambda () (stream-cycle-worker (rest xs) acc))))))

; Prozedur stream-cycle
; - akzeptiert eine Liste xs
; - erzeugt einen Stream, die die Elemente der Liste immer wiederholt
; - bei leerer Liste -> Fehlermeldung
(: stream-cycle ((list-of %a) -> (stream-of %a)))

(check-expect (stream-take 5 (stream-cycle (list 1 2 3))) (list 1 2 3 1 2))
(check-expect (stream-take 6 (stream-cycle (list "a" "b"))) (list "a" "b" "a" "b" "a" "b"))
(check-expect (stream-take 3 (stream-cycle (list (list 1)))) (list (list 1) (list 1) (list 1)))
(check-error (stream-cycle empty) "Liste ist leer")

(define stream-cycle
  (lambda (xs)
    (if (empty? xs)
        (violation "Liste ist leer")
        (stream-cycle-worker xs xs))))

; 4.) Annäherung von pi
(define pi 3.14159265359)

; (a)
; Prozedur series
; - akzeptiert eine Prozedur f und eine natürliche Zahl n
; - erstellt einen Stream aus f(z), f(z+1), f(z+2), ...
(: series ((natural -> real) natural -> (stream-of real)))

(check-expect (stream-take 5 (series (lambda (x) (+ x 1)) 1)) (list 2 3 4 5 6))
(check-expect (stream-take 3 (series (lambda (x) (- x 1)) 0)) (list -1 0 1))
(check-expect (stream-take 10 (series (lambda (x) (* x 2)) 1)) (list 2 4 6 8 10 12 14 16 18 20))



        