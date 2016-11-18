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
                     "Cerebral"
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
; bei 0 kein Besitz des Charakters

(define special-lvl
  (signature (mixed natural (one-of #f))))

; evolve-to soll auf die ID des Charakters hinweisen, zu dem er sich entwickeln kann

; definiere power-type für signature
; bei #f an der Stelle keine Power enthalten

(define pow-type
  (signature (one-of "Charge Specials"
                     "Damage Reduction"
                     "Bind Resistance"
                     "Poison Resistance"
                     "Despair Resistance"
                     "Auto-Heal"
                     "Slot Rate Boost"
                     "Map Damage Resistance"
                     "Resilience"
                     "RCV Boost"
                     "none"
                     #f)))

; definieren der power-count signature

(define pow-count
  (signature (one-of #f 1 2 3 4 5)))

; Record-Procedure über character-powers
; eine character-power besteht aus
; - count
;   bei #f keine power vorhanden
; - type

(: make-powers (pow-count pow-type pow-type pow-type pow-type pow-type -> powers))

(define-record-procedures powers
  make-powers
  powers?
   (powers-count
    powers-type1
    powers-type2
    powers-type3
    powers-type4
    powers-type5))

; Record-Procedure über die Charaktere im Spiel
; Ein Charakter (character) besteht aus:
; - name
; - type
; - class1
; - class2, bei "none" besitzt der Charakter keine 2. Klasse
; - rarity, Seltenheit hat max. 6 Sterne
; - momentanes level
; - maximales level
; - special-level-current
; - special-level-max
; - evolve-to, weist auf die ID eines anderen Charakters
;              bei "none" keine evolve form
; - powers
; - unlock

(: make-character (string char-type classes classes level natural rarity special-lvl special-lvl string powers boolean -> character))

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
(: character-powers (character -> powers))
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
  (make-character "Monkey D. Luffy"
                  "STR" "Fighter" "none"
                  #f 5
                  2
                  0 1
                  "ID2"
                  (make-powers #f #f #f #f #f #f) #t))

(define ID2
  (make-character "Monkey D. Luffy Gum-Gum Pistol"
                  "STR" "Fighter" "none"
                  #f 20
                  3
                  0 1
                  "ID3"
                  (make-powers #f #f #f #f #f #f) #t))

(define ID3
  (make-character "Monkey D. Luffy Gum-Gum Bazooka"
                  "STR" "Fighter" "none"
                  #f 35
                  4
                  0 1
                  "ID4"
                  (make-powers 2 #f #f #f #f #f) #t))

(define ID4
  (make-character "Monkey D. Luffy Gear 2"
                  "STR" "Fighter" "Free Spirit"
                  60 99
                  5
                  0 1
                  "none"
                  (make-powers 3
                               "Map Damage Resistance"
                               "Auto-Heal"
                               "RCV Boost"
                               #f #f) #t))

(define ID5
  (make-character "Roronoa Zoro"
                  "DEX" "Slasher" "none"
                  #f 20
                  3
                  0 1
                  "ID6 ID7"
                  (make-powers #f #f #f #f #f #f) #t))

(define ID6
  (make-character "Roronoa Zoro Three Thousand Worlds"
                  "DEX" "Slasher" "Driven"
                  #f 35
                  4
                  0 1
                  "ID8"
                  (make-powers 2 #f #f #f #f #f) #t))

(define ID7
  (make-character "Roronoa Zoro Pound Phoenix"
                  "DEX" "Slasher" "Driven"
                  #f 50
                  4
                  0 1
                  "ID8"
                  (make-powers 2 #f #f #f #f #f) #t))

(define ID8
  (make-character "Roronoa Zoro Ashura Ichibugin"
                  "DEX" "Slasher" "Driven"
                  80 99
                  5
                  0 1
                  "none"
                  (make-powers 3
                               "Slot Rate Boost"
                               "Damage Reduction"
                               "Resilience"
                               #f #f) #t))

(define ID9
  (make-character "Nami"
                  "INT" "Striker" "none"
                  #f 20
                  3
                  0 1
                  "ID10 ID11"
                  (make-powers #f #f #f #f #f #f) #t))

(define ID10
  (make-character "Nami Tornado Tempo"
                  "INT" "Striker" "Cerebral"
                  #f 35
                  4
                  0 1
                  "ID12"
                  (make-powers 2 #f #f #f #f #f) #t))

(define ID11
  (make-character "Nami Mirage Tempo"
                  "INT" "Striker" "Cerebral"
                  50 50
                  4
                  0 1
                  "ID12"
                  (make-powers 2
                               "Despair Resistance"
                               #f #f #f #f) #t))

(define ID12
  (make-character "Nami Thunderbolt Tempo"
                  "INT" "Striker" "Cerebral"
                  #f 99
                  5
                  0 1
                  "none"
                  (make-powers 3 #f #f #f #f #f) #t))
                  
(define ID13
  (make-character "Usopp"
                  "PSY" "Shooter" "none"
                  #f 20
                  3
                  0 1
                  "ID14 ID15"
                  (make-powers #f #f #f #f #f #f) #t))

(define ID14
  (make-character "Usopp Tabasco Star"
                  "PSY" "Shooter"  "none"
                  #f 35
                  4
                  0 1
                  "ID15 ID16"
                  (make-powers 2 #f #f #f #f #f) #t))

(define ID15
  (make-character "Usopp Golden Pound"
                  "PSY" "Shooter" "none"
                  50 50
                  4
                  0 1
                  "ID16"
                  (make-powers 2
                               "Slot Rate Boost"
                               "Poison Resistance"
                               #f #f #f) #t))

(define ID16
  (make-character "Sogeking"
                  "PSY" "Shooter" "none"
                  5 99
                  5
                  0 1
                  "none"
                  (make-powers 3 #f #f #f #f #f) #t))

(define ID17
  (make-character "Sanji"
                  "QCK" "Fighter" "none"
                  #f 20
                  3
                  0 1
                  "ID18 ID19"
                  (make-powers #f #f #f #f #f #f) #t))

(define ID18
  (make-character "Sanji Plastic Surgery Shot"
                  "QCK" "Fighter" "Powerhouse"
                  #f 35
                  4
                  0 1
                  "ID20"
                  (make-powers 2 #f #f #f #f #f) #t))

(define ID19
  (make-character "Chef Sanji Hot Rock Stew"
                  "QCK" "Fighter" "Powerhouse"
                  #f 50
                  4
                  0 1
                  "ID20"
                  (make-powers 2 #f #f #f #f #f) #t))

(define ID20
  (make-character "Sanji Diable Jambe Flambe"
                  "QCK" "Fighter" "Powerhouse"
                  51 99
                  5
                  0 1
                  "none"
                  (make-powers 3 #f #f #f #f #f) #t))
