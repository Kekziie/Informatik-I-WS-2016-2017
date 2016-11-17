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

; definiere signatur für character-level
; bei #f Charakter nicht im Besitz bzw. Evolved

(define level
  (signature (mixed natural (one-of #f))))

; definiere Anzahl der "Sterne" für Seltenheitswert
; wobei 1 Stern nicht selten bis 6 Sterne sehr selten ist

(define rarity
  (signature (one-of 1 2 3 4 5 6)))

; definieren der Signatur des Special levels
; bei #f besitzt der Charakter keine Special

(define special-lvl
  (signature (mixed natural (one-of #f))))

; evolve-to soll auf die ID des Charakters hinweisen, zu dem er sich entwickeln kann

; definiere power-type für signature

(define pow-type
  (signature (one-of "Charge Specials"
                     "Damage Reduction"
                     "Bind Resistance"
                     "Poison Resistance"
                     "Despair Resistance"
                     "Auto Heal"
                     "Slot Rate Boost"
                     "Map Damage Resistance"
                     "Resilience"
                     "RCV Boost"
                     "none")))

; Record-Procedure über character-powers
; eine character-power besteht aus
; - count
;   bei #f keine power vorhanden
; - type

(: make-powers ((mixed natural (one-of #f)) pow-type -> powers))

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
; - momentanes level
; - maximales level
; - special-level-current 
; - special-level-max
;   -> special-level = 0 means this character doesn't own a special
; - evolve-to
; - powers
; - unlock

(: make-character (string char-type classes classes level natural rarity special-lvl special-lvl string natural boolean -> character))

(: character-name (character -> string))
(: character-type (character -> char-type))
(: character-class1 (character -> classes))
(: character-class2 (character -> classes))
(: character-level (character -> level))
(: character-level-max (character -> natural))
(: character-rarity (character -> rarity))
(: character-special-lvl (character -> special-lvl))
(: character-special-lvl-max (character -> special-lvl))
(: character-evolve-to (character -> string))
(: character-powers (character -> natural))
(: character-unlock (character -> boolean))

(define-record-procedures character
  make-character
  character?
  (character-name
   character-type
   character-class1
   character-class2
   character-level
   character-level-max
   character-rarity
   character-special-lvl
   character-special-lvl-max
   character-evolve-to
   character-powers
   character-unlock))

; ein Charakter soll durch seiner IDxxxx definiert sein

(define ID1
  (make-character "Monkey D. Luffy" "STR" "Fighter" "none" #f 5 2 0 1 "ID2" 0 #t))

