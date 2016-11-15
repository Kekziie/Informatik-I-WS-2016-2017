;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname exercise2-image-chessboard) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; rekonstruieren eines Schachbretts mit Figuren
; bewegen eines Bauern 2 Felder vorwärts

; definieren von Größe, Modus und Farbe eines Feldes
(define size1 20)
(define mode "solid")
(define color1 "white")
(define color2 "black")

(define field1 (rectangle size1 size1 mode color1))
(define field2 (rectangle size1 size1 mode color2))

;definieren von 2 Reihen vom Schachbrett

(define line1
  (beside field1 field2
          field1 field2
          field1 field2
          field1 field2))

(define line2
  (beside field2 field1
          field2 field1
          field2 field1
          field2 field1))

; zusammensetzen der Reihen zu einem Schachbrett
(define chessboard
  (above line1 line2
         line1 line2
         line1 line2
         line1 line2))

; definieren der "Figuren"
; für jedes Team:
; - ein König (king)
; - eine Königin (queen)
; - zwei Türme (rook)
; - zwei Läufer (bishop)
; - zwei Springer (knight)
; - acht Bauern (pawn)

(define size2 10)
(define color3 "red")
(define color4 "blue")

(define figure-team1 (circle size2 mode color3))
(define figure-team2 (circle size2 mode color4))

(define start-game
  (place-image/align
   figure-team1
   40 0 "left" "top"
   chessboard))

start-game

