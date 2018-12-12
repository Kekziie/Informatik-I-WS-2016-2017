;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname maybe-num+animals+shapes) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 1
; Signatur mabe-num mit number oder #f
(define maybe-num
  (signature (mixed number (one-of #f))))

; multipliziert 2 numbers
; falls mind. ein Wert #f -> Ergebnis false
(: maybe-* (maybe-num maybe-num -> maybe-num))

(check-expect (maybe-* 1 2) 2)
(check-expect (maybe-* #f 1) #f)
(check-expect (maybe-* 1 #f) #f)
(check-expect (maybe-* 2 -3) -6)
(check-expect (maybe-* #f #f) #f)

(define maybe-*
  (lambda (x y)
    (if (or (false? x) (false? y))
        #f
        (* x y))))

; wurzelt eine Zahl
; falls der Wert negativ oder #f -> Ergebnis false
(: maybe-sqrt (maybe-num -> maybe-num))

(check-expect (maybe-sqrt 1) 1)
(check-expect (maybe-sqrt 9) 3)
(check-expect (maybe-sqrt #f) #f)
(check-expect (maybe-sqrt -1) #f)

(define maybe-sqrt
  (lambda (x)
    (if (or (false? x) (< x 0))
        #f
        (sqrt x))))

; addiert 2 Zahlen
; falls mind. ein Wert #f -> Ergebnis false
(: maybe-+ (maybe-num maybe-num -> maybe-num))

(check-expect (maybe-+ 1 2) 3)
(check-expect (maybe-+ #f 1) #f)
(check-expect (maybe-+ 1 #f) #f)
(check-expect (maybe-+ 2 -3) -1)
(check-expect (maybe-+ #f #f) #f)

(define maybe-+
  (lambda (x y)
    (if (or (false? x) (false? y))
        #f
        (+ x y))))

; dividiert ein Zahl y von x, also x/y
; falls mind. ein Wert #f oder x=0 -> Ergebnis false
(: maybe-/ (maybe-num maybe-num -> maybe-num))

(check-expect (maybe-/ 1 0) #f)
(check-expect (maybe-/ #f 1) #f)
(check-expect (maybe-/ 1 #f) #f)
(check-within (maybe-/ 2 -3) (/ 2 -3) 0.01)
(check-expect (maybe-/ #f #f) #f)
(check-expect (maybe-/ 2 2) 1)

(define maybe-/
  (lambda (x y)
    (if (or (false? x) (false? y) (= y 0))
        #f
        (/ x y))))

;-----------------------------------------------------
; ANIMALS

; a)
; ein Floh
; Appetit gibt an, wie oft ein Floh zubeißt
(: make-flea (natural -> flea))
(: flea? (any -> boolean))
(: flea-appetite (flea -> natural))

(define-record-procedures flea
  make-flea
  flea?
  (flea-appetite))

; definiere Flöhe
(define florian
  (make-flea 3))

(define mik
  (make-flea 0))

; eine Maus
; Flohbisse, gibt an wie oft von Floh gebissen
(: make-mouse (natural -> mouse))
(: mouse? (any -> boolean))
(: mouse-flea-bite-count (mouse -> natural))

(define-record-procedures mouse
  make-mouse
  mouse?
  (mouse-flea-bite-count))

; definiere Maus
(define jerry
  (make-mouse 3))

(define mickey
  (make-mouse 0))

(define test-maus
  (make-mouse 1000))

; Signatur maybe-victim
; gibt Opfer: Maus an oder kein Opfer #f
(define maybe-victim
  (signature (mixed mouse (one-of #f))))

; eine Katze
; Opfer, gibt an welche Maus sie jagen
; Flohbisse, gibt an wie oft von Floh gebissen
(: make-cat (maybe-victim natural -> cat))
(: cat? (any -> boolean))
(: cat-victim (cat -> maybe-victim))
(: cat-flea-bite-count (cat -> natural))

(define-record-procedures cat
  make-cat
  cat?
  (cat-victim
   cat-flea-bite-count))

; definiere Katze
(define joerg
  (make-cat jerry 3))

(define tom
  (make-cat jerry 2))

; Signatur maybe-friend
; falls Freund gibt Katze an, falls nicht berfreundet gibt #f
(define maybe-friend
  (signature (mixed cat (one-of #f))))

; ein Hund
; Freund von Katze, gibt Katze an, falls nicht befreundet #f
; Flohbisse, gibt an wie oft von Floh gebissen
(: make-dog (maybe-friend natural -> dog))
(: dog? (any -> boolean))
(: dog-friend (dog -> maybe-friend))
(: dog-flea-bite-count (dog -> natural))

(define-record-procedures dog
  make-dog
  dog?
  (dog-friend
   dog-flea-bite-count))

; definieren Hunde
(define Duke
  (make-dog joerg 3))

(define bello
  (make-dog #f 0))

;b)
; Signatur Säugetiere enthält Mäuse Katzen und Hunde
(define mammal
  (signature (mixed mouse cat dog)))

; Signatur Tiere umfasst Säugetiere und Flöhe
(define animal
  (signature (mixed mammal flea)))

; c)
; gibt an, ob es sich um ein Haustier handelt
(: pet? (any -> boolean))

(check-expect (pet? 1) #f)
(check-expect (pet? #t) #f)
(check-expect (pet? jerry) #f)
(check-expect (pet? florian) #f)
(check-expect (pet? joerg) #t)
(check-expect (pet? duke) #t)

(define pet?
  (lambda (a)
    (or (cat? a) (dog? a))))

; gibt Tierlaute zurück, bei einem Säugetier a
(: sound (mammal -> (one-of "wuff" "miau" "piep")))

(check-expect (sound joerg) "miau")
(check-expect (sound jerry) "piep")
(check-expect (sound duke) "wuff")

(define sound
  (lambda (a)
    (cond ((cat? a) "miau")
          ((dog? a) "wuff")
          (else "piep"))))

;-----------------------------------------------------
; SHAPES
; a)
; kartesische Koordinaten (coordinate) bestehen aus x- und y-Koordinate
(: make-coordinate (real real -> coordinate))
(: coordinate? (any -> boolean))
(: coordinate-x (coordinate -> real))
(: coordinate-y (coordinate -> real))

(define-record-procedures coordinate
  make-coordinate
  coordinate?
  (coordinate-x
   coordinate-y))

; definiere Koordinaten
(define P0
  (make-coordinate 0 0))

(define P1
  (make-coordinate 1 1))

(define P2
  (make-coordinate 0 1))

(define P3
  (make-coordinate 1 0))

(define P4
  (make-coordinate -1 0))

(define P5
  (make-coordinate 0 -1))

; b)
; Formen
; ein Kreis besteht aus einer Koordinate und dem Radius
(: make-circ (coordinate real -> circ))
(define-record-procedures circ
  make-circ
  circ?
  (circ-coordinate
   circ-radius))

; definiere Kreise
(define c1
  (make-circ P0 1))

(define c2
  (make-circ P1 2))

; ein Rechteck besteht aus einer Koordinate (am Mittelpunkt) und der Breite und Höhe
(: make-rect (coordinate real real -> rect))
(define-record-procedures rect
  make-rect
  rect?
  (rect-coordinate
   rect-width
   rect-height))

; definiere Rechteckec
(define r1
  (make-rect P0 1 1))

(define r2
  (make-rect P1 2 2))

; ein gleichschenkliges Dreick besteht aus einer Koordinate (Mitte von Untereite vom Dreieck)
; und Breite und Höhe
(: make-tria (coordinate real real -> tria))
(define-record-procedures tria
  make-tria
  tria?
  (tria-coordinate
   tria-width
   tria-height))

; definiere Dreiecke
(define t1
  (make-tria P0 1 1))

(define t2
  (make-tria P1 2 2))

; c)
; Signatur shape beinhaltet Kreise, Rechtecke und gleichschenlige Dreiecke
(define shape
  (signature (mixed circ
                    rect
                    tria)))

; d)
; gibt Basiskoordinate einer Form zurück
(: shape-base (shape -> coordinate))

(check-expect (shape-base c1) P0)
(check-expect (shape-base r1) P0)
(check-expect (shape-base t1) P0)

(define shape-base
  (lambda (sh)
    (cond ((circ? sh) (circ-coordinate sh))
          ((rect? sh) (rect-coordinate sh))
          (else (tria-coordinate sh)))))

; definiere PI
(define pi 3.1415)

; Flächeninhalte einer Form
(: shape-area (shape -> real))

(check-within (shape-area c1) pi 0.01)
(check-expect (shape-area r1) 1)
(check-within (shape-area t1) 0.5 0.01)

(define shape-area
  (lambda (sh)
    (cond ((circ? sh) (* (expt (circ-radius sh) 2) pi))
          ((rect? sh) (* (rect-width sh) (rect-height sh)))
          (else (* 0.5 (tria-width sh) (tria-height sh))))))

; vergleicht den Flächeinhalt von 2 Formen sh1 und sh2
(: shape-compare-area (shape shape -> (one-of "equal" "bigger" "smaller")))

(check-within (shape-compare-area c1 r1) "bigger" 0.01)
(check-within (shape-compare-area r1 c1) "smaller" 0.01)
(check-within (shape-compare-area r1 (make-tria P0 1 2)) "equal" 0.01)

(define shape-compare-area
  (lambda (sh1 sh2)
    (cond ((> (shape-area sh1) (shape-area sh2)) "bigger")
          ((< (shape-area sh1) (shape-area sh2)) "smaller")
          (else "equal"))))

; berechnet Abstand zum Ursprung
(: distance (coordinate -> real))

(check-expect (distance P0) 0)
(check-within (distance P1) (sqrt 2) 0.01)
(check-expect (distance P2) 1)

(define distance
  (lambda (xy)
    (sqrt (+ (expt (coordinate-x xy) 2) (expt (coordinate-y xy) 2)))))

; berechnet Abstand zum Ursprung von einer Form
(: shape-origin-distance (shape -> real))

(check-within (shape-origin-distance t2) (sqrt 2) 0.01)
(check-within (shape-origin-distance r2) (sqrt 2) 0.01)
(check-within (shape-origin-distance c2) (sqrt 2) 0.01)

(define shape-origin-distance
  (lambda (sh)
     (cond ((circ? sh) (distance (circ-coordinate sh)))
           ((rect? sh) (distance (rect-coordinate sh)))
           (else (distance (tria-coordinate sh))))))

; multipliziert einen Wert auf die Größenwerte von Formen (Radius, Breite, Höhe) 
(: scale-shape (shape real -> shape))

(check-expect (scale-shape t1 1) t1)
(check-expect (scale-shape r1 1) r1)
(check-expect (scale-shape c1 1) c1)

(define scale-shape
  (lambda (sh x)
    (cond ((circ? sh) (make-circ (circ-coordinate sh)
                                 (* (circ-radius sh) x)))
          ((rect? sh) (make-rect (rect-coordinate sh)
                                 (* (rect-width sh) x)
                                 (* (rect-height sh) x)))
          (else (make-tria (tria-coordinate sh)
                           (* (tria-width sh) x)
                           (* (tria-height sh) x))))))

; gibt an, Form ein Quadrat ist
(: quadrat? (any -> boolean))

(check-expect (quadrat? r1) #t)
(check-expect (quadrat? 1) #f)
(check-expect (quadrat? #t) #f)
(check-expect (quadrat? c1) #f)
(check-expect (quadrat? (make-rect P0 1 4)) #f)

(define quadrat?
  (lambda (a)
    (and (rect? a) (= (rect-width a) (rect-height a)))))

; gibt Seitenlänge eines Quadrats und Dreiecks an
(: shape-side ((mixed tria (predicate quadrat?)) -> real))

(check-expect (shape-side t1) 1)
(check-expect (shape-side r1) 1)

(define shape-side
  (lambda (sh)
    (if (tria? sh)
        (tria-width sh)
        (rect-width sh))))
    