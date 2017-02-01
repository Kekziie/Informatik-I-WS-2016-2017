;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise11-cup-and-plate) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Übung 11 Tasse und Teller

; Eine Tasse (cup) besteht aus:
; - Volumen (vol)
; - Farbe (col)

(define-record-procedures cup
  make-cup
  cup?
  (cup-vol
   cup-col))

(: make-cup (real string -> cup))
(: cup-vol (cup -> real))
(: cup-col (cup -> string))
(: cup? (any -> boolean))

; Ein Teller (plate) besteht aus:
; - Durchmesser (dia)
; - Farbe (col)

(define-record-procedures plate
  make-plate
  plate?
  (plate-dia
   plate-col))

(: make-col (real string -> plate))
(: plate-dia (plate -> real))
(: plate-col (plate -> string))
(: plate? (any -> boolean))

;--------------------------------------------------------

(define cup1
  (make-cup 10 "blue"))

(define cup2
  (make-cup 15 "blue"))

(define cup3
  (make-cup 20 "red"))

(define plate1
  (make-plate 10 "blue"))

(defin plate2
  (make-plate 15 "blue"))

(define plate3
  (make-plate 20 "red"))

(define plate4
  (make-plate 30 "green"))

; Signatur dishes beinhaltet eine Tasse und Teller
(define dishes
  (signature (mixed cup plate)))

; Prozedur same-dish-color
; - akzeptiert eine Liste von dishes
; - gibt #t wenn alle dishes dieselbe Farbe haben
(: same-dish-color ((list-of dishes) -> boolean))

(check-expect (same-dish-color (list-of