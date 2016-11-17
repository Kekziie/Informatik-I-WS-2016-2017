;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname exercise3-Record-Procedures) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Record-Procedure über das Handyspiel "One Piece: Treasure Cruise"

; definieren aller Eigenschaften eines Charakters

; definiere character-type in signature
; type "all" includes STR DEX QCK INT and PSY

(define char-type
  (signature (one-of "STR"
                     "DEX"
                     "QCK"
                     "INT"
                     "PSY"
                     "all"
                     "Rainbow")))

; definiere character-classes in signature

(define classes
  (signature (one-of "Fighter"
                     "Slasher"
                     "Striker"
                     "Driven"
                     "Free Spirit"
                     "Evolver"
                     "Booster"
                     "Powerhouse"
                     "none")))

; definiere Anzahl der "Sterne" für Seltenheitswert
; wobei 1 Stern nicht selten bis 6 Sterne sehr selten ist

(define rarity
  (signature (one-of 1 2 3 4 5 6)))

; evolve-to soll auf die ID des Charakters hinweisen, zu dem er sich entwickeln kann

; definiere power-type für signature

(define pow-type
  (signature (one-of "Charge Special"
                     "Bind Resistance"
                     "Poison Resistance"
                     "Map Damage Resistance"
                     "Resilience"
                     "RCV Boost"
                     "none")))

; Record-Procedure über character-powers
; eine character-power besteht aus
; - count
;   -> count=0 means character have no powers
; - type

(: make-powers (natural pow-type -> powers))

(define-record-procedures powers
  make-powers
  powers?
   (powers-count
    powers-tye))

(define powers-ID1
  (make-powers 0 "none"))

; Record-Procedure über die Charaktere im Spiel
; Ein Charakter (character) besteht aus:
; - name
; - type
; - class1
; - class2
; - rarity
; - special-level-current 
; - special-level-max
;   -> special-level = 0 means this character doesn't own a special
; - evolve-to
; - powers

(: make-character (string char-type classes classes rarity natural natural string natural -> character))

(define-record-procedures character
  make-character
  character?
  (character-name
   character-type
   character-class1
   character-class2
   character-rarity
   character-special-lvl
   character-special-lvl-max
   character-evolve-to
   character-powers))

; ein Charakter soll durch seiner IDxxxx definiert sein

(define ID1
  (make-character "Luffy" "STR" "Fighter" "none" 2 0 1 "ID2" 0))

