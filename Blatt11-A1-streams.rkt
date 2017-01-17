;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt11-A1-streams) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1

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
    (make-cons n
               (lambda () (const-stream n)))))