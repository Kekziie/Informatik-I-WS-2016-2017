;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt04-A5-soccer) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 5

; ordnet einer gegebenen Rückennummer eine Position beim Fußball zu
; Annahme : - Torwart Nr.1
;           - Abwehrspieler Nr.2 3 4 5
;           - Mittelfeldspieler Nr. 6 7 8 10
;           - Stürmer Nr. 9 11
;           - Ersatzspieler Nr. 13 99
;           - restliche Nummern ungültig

(: player-position (natural -> (one-of "Torwart" "Abwehr" "Mittelfeld" "Sturm" "Ersatz" "Ungültig")))

(check-expect (player-position 0) "Ungültig")
(check-expect (player-position 1) "Torwart")
(check-expect (player-position 2) "Abwehr")
(check-expect (player-position 6) "Mittelfeld")
(check-expect (player-position 10) "Mittelfeld")
(check-expect (player-position 9) "Sturm")
(check-expect (player-position 11) "Sturm")
(check-expect (player-position 33) "Ersatz")
(check-expect (player-position 12) "Ungültig")
(check-expect (player-position 100) "Ungültig")

(define player-position
  (lambda (x)
    (cond
      ((= x 1) "Torwart")
      ((<= 2 x 5) "Abwehr")
      ((<= 6 x 8) "Mittelfeld")
      ((= x 10) "Mittelfeld")
      ((= x 9) "Sturm")
      ((= x 11) "Sturm")
      ((<= 13 x 99) "Ersatz")
      (else "Ungültig"))))