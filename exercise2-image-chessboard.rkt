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
; - ein König (king K)
; - eine Königin (queen Q)
; - zwei Türme (rook R)
; - zwei Läufer (bishop B)
; - zwei Springer (knight Kn)
; - acht Bauern (pawn P)

; definieren von Größe und Farben der "Figuren"
(define size2 10)
(define color3 "red")
(define color4 "blue")

(define figure1 (circle size2 mode color3))
(define figure2 (circle size2 mode color4))

; definieren einer Schriftgröße, Farbe und einer Position

(define size3 15)
(define mid "middle")
(define text-color "black")

; definieren eines "Aufkleber"-Umrisses

(define sticker (circle size2 "outline" text-color))

; definieren eines "Aufkleber"-Sets an verschiedenen Firguren

(define king
  (overlay/align mid mid
                 (text "K" size3 text-color)
                 sticker))

(define queen
  (overlay/align mid mid
                 (text "Q" size3 text-color)
                 sticker))

(define rook
  (overlay/align mid mid
                 (text "R" size3 text-color)
                 sticker))

(define bishop
  (overlay/align mid mid
                 (text "B" size3 text-color)
                 sticker))

(define knight
  (overlay/align mid mid
                 (text "Kn" size3 text-color)
                 sticker))

(define pawn
  (overlay/align mid mid
                 (text "P" size3 text-color)
                 sticker))




