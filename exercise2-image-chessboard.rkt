;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname exercise2-image-chessboard) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; rekonstruieren eines Schachbretts mit Figuren
; bewegen eines Springers 2 Felder vorwärts ein Feld rechts


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
(define chessboard-field
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

(define king-sticker
  (overlay/align mid mid
                 (text "K" size3 text-color)
                 sticker))

(define queen-sticker
  (overlay/align mid mid
                 (text "Q" size3 text-color)
                 sticker))

(define rook-sticker
  (overlay/align mid mid
                 (text "R" size3 text-color)
                 sticker))

(define bishop-sticker
  (overlay/align mid mid
                 (text "B" size3 text-color)
                 sticker))

(define knight-sticker
  (overlay/align mid mid
                 (text "Kn" size3 text-color)
                 sticker))

(define pawn-sticker
  (overlay/align mid mid
                 (text "P" size3 text-color)
                 sticker))

; erstellen der Figuren Team 1

(define king1
  (overlay king-sticker
           figure1))

(define queen1
  (overlay queen-sticker
           figure1))

(define rook1
  (overlay rook-sticker
           figure1))

(define bishop1
  (overlay bishop-sticker
           figure1))

(define knight1
  (overlay knight-sticker
           figure1))

(define pawn1
  (overlay pawn-sticker
           figure1))

; erstellen der Figuren Team 2

(define king2
  (overlay king-sticker
           figure2))

(define queen2
  (overlay queen-sticker
           figure2))

(define rook2
  (overlay rook-sticker
           figure2))

(define bishop2
  (overlay bishop-sticker
           figure2))

(define knight2
  (overlay knight-sticker
           figure2))

(define pawn2
  (overlay pawn-sticker
           figure2))

; erstellen einer "Bauern"-Reihe

(define pawn1-row
  (beside pawn1 pawn1 pawn1 pawn1
          pawn1 pawn1 pawn1 pawn1))

(define pawn2-row
  (beside pawn2 pawn2 pawn2 pawn2
          pawn2 pawn2 pawn2 pawn2))

; erstellen der hinteren Figuren Reihe Team 1

(define backrow1
  (beside rook1 knight1 bishop1 king1
          queen1 bishop1 knight1 rook1))

; erstellen der hinteren Figuren Reihen Team 2 ohne einen Springer

(define backrow2
  (beside rook2 field1 bishop2 king2
          queen2 bishop2 knight2 rook2))

; erstellen eines Schachspiels beim Start
; da nur der Springer sich bewegen soll, "festsetzen" der anderen Figuren auf Schachbrett

(define chessboard-start
  (place-image/align
    backrow1
    0 0 "left" "top"
  (place-image/align
    pawn1-row
    0 20 "left" "top"
  (place-image/align
    pawn2-row
    0 120 "left" "top"
  (place-image/align
    backrow2
    0 140 "left" "top"
    chessboard-field)))))

; erstellen des ersten Zuges
; der knight des unteren team läuft 2 felder vorwärts und eins nach rechts
(: first-move (natural -> image))

(define first-move
  (lambda (t)
    (clear-pinhole
     (overlay/pinhole (knight t)
                      (chessboard t)))))

; definieren der Position x0 zur Position x1 vom Springer
(: knight (natural -> image))

(define knight
  (lambda (t)
    (put-pinhole (move-figure 60 40 t)
                 (move-figure -60 -20 t)
                 knight2)))

; definieren der Bewegung der Figur in max t=25
(: move-figure (integer integer natural -> integer))

(check-expect (move-figure -10 10 0) -10)
(check-expect (move-figure -10 10 25) 10)
(check-expect (move-figure -10 10 50) 10)

(define move-figure
  (lambda (x0 x1 t)
    (+ x0
       (floor (* (min t 25)
                 (/ (- x1 x0)
                    25))))))

; definieren des Schachbretts
(define chessboard
  (lambda (t)
    chessboard-start))

; Zeigt 2 Bilder
(first-move 0) ; zeigt Springer an Ursprungsposition bei t=0
(first-move 25) ; zeigt Springer nach ersten Zug an Position x1 bei t=25

; Animation des ersten Zuges

(animate first-move)