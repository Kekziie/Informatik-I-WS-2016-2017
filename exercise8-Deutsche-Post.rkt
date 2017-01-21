;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise8-Deutsche-Post) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Übung 8 Deutsche Post 2107

; ein Maß (size) besteht aus:
; - Länge (length)
; - Breite (width)
; - Höhe (height)
(: make-size (real real real -> size))
(: size-length (size -> real))
(: size-width (size -> real))
(: size-height (size -> real))

(define-record-procedures size
  make-size
  size?
  (size-length
   size-width
   size-height))

; ein Brief (letter) besteht aus:
; - Name (name)
; - Gewicht (weight)
; - Maße (size)
; - Preis (price)
(: make-letter (string real size real -> letter))
(: letter-name (letter -> string))
(: letter-weight (letter -> real))
(: letter-size (letter -> size))
(: letter-price (letter -> real))

(define-record-procedures letter
  make-letter
  letter?
  (letter-name
   letter-weight
   letter-size
   letter-price))
