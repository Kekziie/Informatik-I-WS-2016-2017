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
  (make-letter "postcard" 15  (make-size 23.5 12.5 0) 0.45))

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

;========================================================================

; (a)
; Prozedur which-one1?
; akzeptiert eine Größe s
; soll Angebot raussuchen,dass zur Größe passt
; wenn mehrere Angebote passen, dann soll das billigste wiedergegegben werden
; wenn  kein Angebot passt, soll eine Fehlermedlung zurückgegeben werden
(: which-one1? (size -> (mixed letter shipment)))

(check-expect (which-one1? (make-size 12 12 0.5)) standard-letter)
(check-expect (which-one1? (make-size 25 25 1)) large-letter)
(check-expect (which-one1? (make-size 10 10 0)) postcard)
(check-expect (which-one1? (make-size 35 20 3)) Maxi-letter)
(check-expect (which-one1? (make-size 10 10 15)) compact)
(check-expect (which-one1? (make-size 20 30 12)) compact)
(check-expect (which-one1? (make-size 20 12 1)) compact-letter)
(check-error (which-one1? (make-size 50 50 50)) "kein Angebot passt")

(define which-one1?
  (lambda (s)
   (let ((length (size-length s))
         (width (size-width s))
         (height (size-height s)))
    (cond
      ((and (= height 0)
            (<= length 23.5)
            (<= width 12.5)) postcard)
      ((and (<= height 0.5)
            (<= length 23.5)
            (<= width 12.5)) standard-letter)
      ((and (<= height 1)
            (<= length 23.5)
            (<= width 12.5)) compact-letter)
      ((and (<= height 2)
            (<= length 35.3)
            (<= width 25)) large-letter)
      ((and (<= height 5)
            (<= length 35.3)
            (<= width 25)) Maxi-letter)
      ((and (<= height 15)
            (<= length 35.3)
            (<= width 30)) compact)
      (else (violation "kein Angebot passt"))))))
      
; (b)
; Proezdur which-one2?
; akzeptiert ein Gewicht w
; soll Angebot raussuchen,dass zum Gewicht passt
; wenn mehrere Angebote passen, dann soll das billigste wiedergegegben werden
; wenn  kein Angebot passt, soll eine Fehlermedlung zurückgegeben werden
(: which-one2? (real -> (mixed letter shipment)))

(check-expect (which-one2? 15) postcard)
(check-expect (which-one2? 20) standard-letter)
(check-expect (which-one2? 50) compact-letter)
(check-expect (which-one2? 200) large-letter)
(check-expect (which-one2? 1000) Maxi-letter)
(check-error (which-one2? 2000) "kein Angebot passt")

(define which-one2?
  (lambda (w)
    (cond
      ((<= w 15) postcard)
      ((<= w 20) standard-letter)
      ((<= w 50) compact-letter)
      ((<= w 500) large-letter)
      ((<= w 1000) Maxi-letter)
      (else (violation "kein Angebot passt")))))
            
    