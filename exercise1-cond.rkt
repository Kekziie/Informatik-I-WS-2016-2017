;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname exercise1-cond) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Bewertungsschema einer Klausur
; jeder Punktzahl soll einer Note zugeordnet werden
; mind. 90 Punkte für ein "sehr gut", alle 10 Punkte darunter ein Note schlechter

(define Note
  (signature (one-of "sehr gut"
                     "gut"
                     "befriedigend"
                     "ausreichend"
                     "mangelhaft"
                     "ungenügend")))

(: Bewertung (natural -> Note))

(check-expect (Bewertung 100) "sehr gut")
(check-expect (Bewertung 88) "gut")
(check-expect (Bewertung 71) "befriedigend")
(check-expect (Bewertung 60) "ausreichend")
(check-expect (Bewertung 52) "mangelhaft")
(check-expect (Bewertung 0) "ungenügend")

              