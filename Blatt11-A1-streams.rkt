;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt11-A1-streams) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1
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


; (a) Prozedur "const-stream"
; - akzeptiert beliebigen Wert x
; - gibt einen konstanten unendlichen Strom vom Wert zurück

(: const-stream (%a -> (stream-of %a)))

(check-expect (stream-take 0 (const-stream 1)) empty)
(check-expect (stream-take 5 (const-stream 1)) (list 1 1 1 1 1))
(check-expect (stream-take 10 (const-stream 2)) (list 2 2 2 2 2 2 2 2 2 2))
(check-expect (stream-take 3 (const-stream "a")) (list "a" "a" "a"))
(check-expect (stream-take 2 (const-stream #f)) (list #f #f))

(define const-stream
  (lambda (x)
    (make-cons x
               (lambda () (const-stream x)))))

; "ones" ist ein Strom von Einsen
(check-expect (stream-take 3 ones) (list 1 1 1))
(check-expect (stream-take 5 ones) (list 1 1 1 1 1))

(define ones
  (const-stream 1))
