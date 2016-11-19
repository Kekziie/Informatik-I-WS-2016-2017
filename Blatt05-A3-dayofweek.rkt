;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt05-A3-dayofweek) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 3

; ersetzen, der in der Signatur vorkommende (one-of ...) in andere gleichwertige Konstrukte

(define Mo "Montag")
(define Di "Dienstag")
(define Mi "Mittwoch")
(define Do "Donnerstag")
(define Fr "Freitag")
(define Sa "Samstag")
(define So "Sonntag")

; ermittelt die Nummer eines Wochentages innerhalb einer Woche
; die Nummerierung beginnt mit 0 und nimmt Montag als ersten Tag der Woche an

(: wochentag-index ((one-of Mo Di Mi Do Fr Sa So) -> (one-of 0 1 2 3 4 5 6)))

(define wochentag-index
  (lambda (tag)
    (cond
      ((string=? tag Mo) 0)
      ((string=? tag Di) 1)
      ((string=? tag Mi) 2)
      ((string=? tag Do) 3)
      ((string=? tag Fr) 4)
      ((string=? tag Sa) 5)
      ((string=? tag So) 6)
      )))