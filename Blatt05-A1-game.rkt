;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt05-A1-game) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1

; Ausschnitt eines Computerspiels mit:
; - Spielfigur (character) besteht aus: Name, ganzzahliger Gesundheitszustand (0-100) und zweidimensionale Position
; - Bomben (bomb) bestehen aus: Detonationsradius ("blast radius") und Schadenswert
; - Bombenangriff (attack) besteht aus: Position und Bombe

; Treffer bei Bombenangriff: Abstand zur Bombe kleiner als Detonationsradius
;                            Gesundheitszustand verringert

; Schaden der Bombe: verringert bei zunehmender Entfernung des Detonationsradiuses
;                    Reduzieung des Gesundheitszustand um folgenden Wert: (1 - D/R) * S
;                    wobei D Distanz, R Detonationsradius (d<r) und S Schadenswert sind

; Spielfigur: erleidet keinen Schaden, wenn nicht im Detonationsradius
;             Gesundheitszustand darf nicht kleiner als 0 sein

; (a) Daten- und Recorddefinition für: x/y-Position, Spielfigur, Bomben und Bombenabwürfe

; Recorddefinition für x/y-Position
; x/y-Position besteht aus x-Koordinate und y-Koordinate
(: make-position (real real -> position))
(: position? (any -> boolean))
(: position-x (position -> real))
(: position-y (position -> real))

(check-property
 (for-all ((x real)
           (y real))
   (and (= (position-x (make-position x y)) x)
        (= (position-y (make-position x y)) y))))

(define-record-procedures position
  make-position
  position?
  (position-x
   position-y))

; definieren einer Gesundheitsprozedur mit Signatur
(: healthbar (natural -> natural))

(check-expect (healthbar 0) 0)
(check-expect (healthbar 100) 100)
(check-error (healthbar 101) "max health is 100!")

(define healthbar
  (lambda (x)
    (if (<= 0 x 100)
        x
        (violation "max health is 100!"))))

(define maxhealth
  (signature (predicate healthbar)))

; Recorddefiniton für Spielfigur
; Spielfigur besteht aus Name n, Gesundheitszustand h und Position x y
(: make-character (string maxhealth position -> character))
(: character? (any -> boolean))
(: character-name (character -> string))
(: character-health (character -> maxhealth))
(: character-position (character -> position))

(check-expect (character-name Spielfigur1) "Mario")
(check-expect (character-health Spielfigur2) 100)
(check-expect (character-position Spielfigur2) Position1) 

(define-record-procedures character
  make-character
  character?
  (character-name
   character-health
   character-position))

; Recorddefinition für Bombe
; Bomben bestehen aus Detonationsradius br und Schaden d
(: make-bomb (real real -> bomb))
(: bomb? (any -> boolean))
(: bomb-blast-radius (bomb -> real))
(: bomb-damage (bomb -> real))

(check-property
 (for-all ((br real)
           (d real))
   (and (= (bomb-blast-radius (make-bomb br d)) br)
        (= (bomb-damage (make-bomb br d)) d))))

(define-record-procedures bomb
  make-bomb
  bomb?
  (bomb-blast-radius
   bomb-damage))

; Recorddefinition für Bomben-Angriff
; Angriff besteht aus Position x y und Bombe br d
(: make-attack (position bomb -> attack))
(: attack? (any -> boolean))
(: attack-position (attack -> position))
(: attack-bomb (attack -> bomb))

(check-expect (attack-position Angriff2) Position2)
(check-expect (attack-bomb Angriff2) Bombe1)

(define-record-procedures attack
  make-attack
  attack?
  (attack-position
   attack-bomb))

; Datendefinition

(define Position1
  (make-position 30 33))

(define Position2
  (make-position 15 10))

(define Position3
  (make-position -5 0))

(define Spielfigur1
  (make-character "Mario" 100 (make-position 10 15)))

(define Spielfigur2
  (make-character "Lan" 100 Position1))

(define Spielfigur3
  (make-character "Herbie" 100 (make-position -100 -100)))

(define Bombe1
  (make-bomb 50 25))

(define Bombe2
  (make-bomb 25 25))

(define Angriff1
  (make-attack (make-position 60 30) (make-bomb 15 15)))

(define Angriff2
  (make-attack Position2 Bombe1))

(define Angriff3
  (make-attack Position2 (make-bomb 50 1)))

(define Angriff4
  (make-attack (make-position 29 31) (make-bomb 100 10000)))

; (b) euklidische Distanz d zwischen zwei Positionen: d= Wurzel aus {(x1-x2)'2 + (y1-y2)'2}

(: euclidean-distance (position position -> real))

(check-within (euclidean-distance position1 position2) 27.46 0.01)
(check-within (euclidean-distance (character-position Spielfigur1) position1) 26.91 0.01)
(check-within (euclidean-distance (character-position Spielfigur2) (attack-position Angriff1)) 30.15 0.01)

(define euclidean-distance
  (lambda (position1 position2)
    (sqrt (+ (expt (- (position-x position1)
                   (position-x position2))
                2)
             (expt (- (position-y position1)
                      (position-y position2))
                   2)))))

; (c) eine Prozedur drop-bomb, die alle Auswirkungen eines Bombenabwurfs auf eine Spielfigur berechnet

; definieren des Schadenwertes, wenn Spielfigur getroffen

(: damage (character attack -> real))

(check-within (damage Spielfigur1 Angriff2) 21.0 0.001)
(check-within (damage Spielfigur2 Angriff2) 11.0 0.001)

(define damage
  (lambda (Spielfigur Angriff)
     (round (* (- 1 (/ (euclidean-distance (character-position Spielfigur)
                                           (attack-position Angriff))
                       (bomb-blast-radius (attack-bomb Angriff))))
               (bomb-damage (attack-bomb Angriff))))))

; Prozedur: drop-bomb, die Schaden an Figur berechnet
; bei keinem Treffer: "daneben"

(: drop-bomb (character attack -> character))

(check-within (drop-bomb Spielfigur1 Angriff2) (make-character "Mario" 79 (make-position 10 15)) 0.001)
(check-within (drop-bomb  Spielfigur2 Angriff2) (make-character "Lan" 89 Position1) 0.001)
(check-within (drop-bomb Spielfigur2 Angriff3) (make-character "Lan" 100 Position1) 0.001)
(check-within (drop-bomb Spielfigur2 Angriff4) (make-character "Lan" 0 Position1) 0.001)
(check-error (drop-bomb  Spielfigur2 Angriff1) "daneben")
(check-error (drop-bomb  Spielfigur3 Angriff1) "daneben")

(define drop-bomb
  (lambda (Spielfigur Angriff)
    (if (< (euclidean-distance (character-position Spielfigur)
                               (attack-position Angriff))
           (bomb-blast-radius (attack-bomb Angriff)))
        (if (>= (- (character-health Spielfigur)
                  (damage Spielfigur Angriff))
                 0)
            (make-character (character-name Spielfigur)
                            (- (character-health Spielfigur)
                               (damage Spielfigur Angriff))
                            (character-position Spielfigur))
            (make-character (character-name Spielfigur)
                            (- (character-health Spielfigur) 100)
                            (character-position Spielfigur)))
        (violation "daneben"))))
        
  