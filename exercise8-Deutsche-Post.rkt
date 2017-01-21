;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise8-Deutsche-Post) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Übung 8 Deutsche Post 2107

; ein max. Maß (size) besteht aus:
; - Länge (length) in cm
; - Breite (width) in cm
; - Höhe (height) in cm
(: make-size (real real real -> size))
(: size-length (size -> real))
(: size-width (size -> real))
(: size-height (size -> real))
(: size? (any -> boolean))

(define-record-procedures size
  make-size
  size?
  (size-length
   size-width
   size-height))

; ein Brief (letter) besteht aus:
; - Name (name)
; - max. Gewicht (max-weight) in g
; - max. Maße (size)
; - Preis (price) in €
(: make-letter (string real size real -> letter))
(: letter-name (letter -> string))
(: letter-max-weight (letter -> real))
(: letter-size (letter -> size))
(: letter-price (letter -> real))
(: letter? (any -> boolean))

(define-record-procedures letter
  make-letter
  letter?
  (letter-name
   letter-max-weight
   letter-size
   letter-price))

; eine Warensendung (shipment) besteht aus:
; - Name (name)
; - max. Gewicht (max-weight) in g
; - max. Maße (size)
; - Preis (price) in €
(: make-shipment (string real size real -> shipment))
(: shipment-name (shipment -> string))
(: shipment-max-weight (shipment -> real))
(: shipment-size (shipment -> size))
(: shipment-price (shipment -> real))
(: shipment? (any -> boolean))

(define-record-procedures shipment
  make-shipment
  shipment?
  (shipment-name
   shipment-max-weight
   shipment-size
   shipment-price))

; Datendefinitionen
; Briefe
(define postcard
  (make-letter "postcard" 150  (make-size 23.5 12.5 0) 0.45))

(define standard-letter
  (make-letter "standard-letter" 20 (make-size 23.5 12.5 0.5) 0.70))

(define compact-letter
  (make-letter "compact-letter" 50 (make-size 23.5 12.5 1) 0.85))

(define large-letter
  (make-letter "large-letter" 500 (make-size 35.3 25 2) 1.45))

(define Maxi-letter
  (make-letter "Maxi-letter" 1000 (make-size 35.3 25 5) 2.60))

;--------------------------------------------------------------
; Warensendungen
(define compact
  (make-shipment "compact" 50 (make-size 35.3 30 15) 0.90))

(define large
  (make-shipment "large" 500 (make-size 35.3 30 15) 1.90))

(define maxi-to-5
  (make-shipment "maxi-to-5" 1000 (make-size 35.3 30 5) 2.20))

(define maxi-to-15
  (make-shipment "maxi-to-15" 1000 (make-size 35 30 15) 2.35))

;====================================================================
; Testfälle
(define Test-Size1
  (make-size 12 12 0.5))

(define Test-Size2
  (make-size 25 25 1))

(define Test-Size3
  (make-size 10 10 0))

(define Test-Size4
  (make-size 35 20 3))

(define Test-Size5
  (make-size 10 10 15))

(define Test-Size6
  (make-size 20 30 12))

;========================================================================

; (a)
; Prozedur which-one?
; akzeptiert eine Größe s
; soll Angebot raussuchen,dass zur Größe passt
; wenn mehrere Angebote passen, dann soll das billigste wiedergegegben werden
; wenn  kein Angebot passt, soll eine Fehlermedlung zurückgegeben werden
(: which-one? (size -> (mixed letter shipment)))

(check-expect (which-one? Test-Size1) standard-letter)
(check-expect (which-one? Test-Size2) large-letter)
(check-expect (which-one? Test-Size3) postcard)
(check-expect (which-one? Test-Size4) Maxi-letter)
(check-expect (which-one? Test-Size5) compact)
(check-expect (which-one? Test-Size6) compact)

(define which-one?
  (lambda (s)
    (cond
      ((and (= (size-height s) 0)
            (<= (size-length s) 23.5)
            (<= (size-width s) 12.5)) postcard)
      ((and (<= (size-height s) 0.5)
            (<= (size-length s) 23.5)
            (<= (size-width s) 12.5)) standard-letter)
      ((and (<= (size-height s) 1)
            (<= (size-length s) 23.5)
            (<= (size-width s) 12.5)) compact-letter)
      ((and (<= (size-height s) 2)
            (<= (size-length s) 35.3)
            (<= (size-width s) 25)) large-letter)
      ((and (<= (size-height s) 5)
            (<= (size-length s) 35.3)
            (<= (size-width s) 25)) Maxi-letter)
      ((and (<= (size-height s) 15)
            (<= (size-length s) 35.3)
            (<= (size-width s) 30)) compact)
      (else (violation "kein Angebot passt")))))
      
      
            
    