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