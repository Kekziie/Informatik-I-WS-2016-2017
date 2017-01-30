;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise10-ISBN) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm") (lib "starwars.rkt" "installed-teachpacks"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm") (lib "starwars.rkt" "installed-teachpacks")))))
; Übung 10 ISBN Check

; Implementieren einer Prozedur die eine 13-stellige Zahl überprüft
; Verfahren:
; 1.) Summe aus dreifachen Wert der Ziffern an geraden Stellen
; 2.) Summe aus Wert der Ziffern an ungeraden Stellen
; 3.) Summe aus 1.) und 2.)
; 4.) Division durch 10
; 5.) wenn Rest 0 -> ISBN #t

;==========================================================================
; Hilfsfunktionen:

; (a)
; Prozedur list-sum
; - akzeptiert eine Liste von Zahlen
; - summiert alle Elemente der Liste zu einem Ergebnis
(:list-sum ((list-of number) -> number))

(check-expect (list-sum (list 1 2 3 4 5)) 15)
(check-expect (list-sum empty) 0)
(check-expect (list-sum (list 1 2 3)) 6)
(check-expect (list-sum (list -1 0 1)) 0)
(check-within (list-sum (list -2.5 2.5 1/2 -1/2 42)) 42 0.01)

(define list-sum
  (lambda (xs)
    (cond
      ((empty? xs) 0)
      ((pair? xs) (+ (first xs)
                     (list-sum (rest xs)))))))

; (b)
; Prozedur mult
; - akzeptiert eine natürliche Zahl x und eine Liste von Zahlen
; - multipliziert jedes Listenelement mit übergebenen Zahl x
(: mult (natural (list-of number) -> (list-of number)))

(check-expect (mult 2 (list 1 2 3)) (list 2 4 6))
(check-expect (mult 2 empty) empty)
(check-expect (mult 3 (list 0 -1 1)) (list 0 -3 3))
(check-expect (mult 0 (list 1 2 3 4 5 6 7 8 9)) (list 0 0 0 0 0 0 0 0 0))
(check-within (mult 1 (list 2.5 -2.5)) (list 2.5 -2.5) 0.01)

(define mult
  (lambda (x xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (make-pair (* (first xs) x)
                             (mult x (rest xs)))))))

; (c)
; Prozedur drop
; - akezeptiert eine natürliche Zahl n und eine beliebige Liste xs
; - verwirft n Elemente der Liste xs
(: drop (natural (list-of %a) -> (list-of %a)))

; (d)
; Prozedur every-nth
; - akzeptiert eine natürliche Zahl n und eine beliebige Liste xs
; - nimmt jedes n-te Element der Liste xs und erstell daraus eine neue Liste
(: every-nth (natural (list-of %a) -> (list-of %a)))

; (e)
; Signatur (list-of-13 t)
; - überprüft, ob Liste 13 Elemente besitzt, die alle Sigantur t haben


;==========================================================================
; Teilaufgabe (f) soll obiges Verfahren realisieren

; (f)
; Prozedur ISBN-Check
; - akzeptiert eine Liste mit 13 ELementen, die alle Signatur natural haben
; - bei #t -> ISBN-Nummer, sonst #f
(: ISBN-Check ((list-of-13 natural) -> boolean))
