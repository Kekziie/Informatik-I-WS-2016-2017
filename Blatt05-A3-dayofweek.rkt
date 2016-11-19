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

; definieren von Fehlermeldungen

(define fehler1 "string ist kein Wochentag")
(define fehler2 "natural befindet sich nicht zwischen 0 und 6")

; definieren einer Prozdur, ob String ein Wochentag entspricht

(: wochentag? (string -> string))

(check-expect (wochentag? "Montag") Mo)
(check-expect (wochentag? "Dienstag") Di)
(check-expect (wochentag? "Mittwoch") Mi)
(check-expect (wochentag? "Donnerstag") Do)
(check-expect (wochentag? "Freitag") Fr)
(check-expect (wochentag? "Samstag") Sa)
(check-expect (wochentag? "Sonntag") So)
(check-error (wochentag? "Wochenende") fehler1)

(define wochentag?
  (lambda (y)
    (cond
      ((string=? y Mo) Mo)
      ((string=? y Di) Di)
      ((string=? y Mi) Mi)
      ((string=? y Do) Do)
      ((string=? y Fr) Fr)
      ((string=? y Sa) Sa)
      ((string=? y So) So)
      ((string=? y y) (violation fehler1)))))

; definieren eienr Wochentags-Signatur

(define wochentag
  (signature (one-of Mo Di Mi Do Fr Sa So)))

; definieren einer Prozedur die nur Zahlen von 0-6 erlaubt
(: 0-to-6 (natural -> natural))

(check-expect (0-to-6 0) 0)
(check-expect (0-to-6 4) 4)
(check-error (0-to-6 7) fehler2)

(define 0-to-6
  (lambda (x)
    (if (<= 0 x 6)
        x
        (violation fehler2))))

; definieren einer Index-Signatur

(define index
  (signature (predicate 0-to-6)))

; ermittelt die Nummer eines Wochentages innerhalb einer Woche
; die Nummerierung beginnt mit 0 und nimmt Montag als ersten Tag der Woche an

(: wochentag-index (wochentag -> index))

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