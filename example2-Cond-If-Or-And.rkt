;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname example2-Cond-If-Or-And) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm")))))
; Beispiele 2

; i) Prozedur mit if
; Prozedur liefert: 
; - falls (>= n 0) -> #t, dann wird der string "positive" zurückgegeben
; - falls (>= n 0) -> #f, dann wird "negative" zurückgegeben
(: negative-or-positive (number -> string))

(check-expect (negative-or-positive 0) "positive")
(check-expect (negative-or-positive -3) "negative")
(check-expect (negative-or-positive -10) "negative")
(check-expect (negative-or-positive 1/2) "positive")
(check-within (negative-or-positive (sqrt 3)) "positive" 0.01)

(define negative-or-positive
  (lambda (n)
    (if (>= n 0)
        "positive"
        "negative")))

; ii) Prozedur mit cond mit Verwendung von and
; Prozedur überprüft, ob gegebene Zahl n positiv und gerade
(: even-and-positive? (number -> boolean))

(check-expect (even-and-positive? 2) #t)
(check-expect (even-and-positive? -4) #f)
(check-expect (even-and-positive? 0) #f)
(check-expect (even-and-positive? 1) #f)

(define even-and-positive?
  (lambda (n)
    (cond
      ((and (even? n) (positive? n)) #t)
      (else #f))))