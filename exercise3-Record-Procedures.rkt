;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname exercise3-Record-Procedures) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Record-Procedure über das Handyspiel "One Piece: Treasure Cruise"

; Record-Procedure über die Charaktere im Spiel
; Ein Charakter (character) besteht aus:
; - ID
; - name
; - type
; - class1
; - class2
; - rarity
; - special-level-current
; - special-level-max
; - evolve-to
; - powers

(: make-character (natural string string string string natural natural natural natural natural -> character))

(define-record-procedures character
  make-character
  character?
  (character-ID
   character-name
   character-type
   character-class1
   character-class2
   character-rarity
   character-special-lvl
   character-special-lvl-max
   character-evolve-to
   character-powers))

; Record-Procedure über character-powers
; eine character-power besteht aus
; - count
; - type

(: make-powers (natural string -> powers))

(define-record-procedures powers
  make-powers
  powers?
  (powers-count
   powers-tye))
