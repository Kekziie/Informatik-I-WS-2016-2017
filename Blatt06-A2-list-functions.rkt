;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt06-A2-list-functions) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 2

; (a)
; Daten- und Recorddefinition für polymorphe Paare
; ein polymorphes Paar (pair) besteht aus
; - erste Komponente (first)
; - zweite Komponente (rest)
; Hinweis: Komponenten können beliebige Werte annehmen
(define-record-procedures-parametric pair pair-of
  make-pair
  pair?
  (first
   rest))

; Datendefinitionen
(define Liste123
  (make-pair 1
             (make-pair 2
                        (make-pair 3
                                   empty))))
(define Wochenende
  (make-pair "Freitag"
             (make-pair "Samstag"
                        (make-pair "Sonntag"
                                   empty))))
(define Wahrheitswerte
  (make-pair #t
             #f))

; Signatur von list-of
; eine Liste kann:
;  - leer sein (empty-list)
;  - aus einem Paar (pair-of) bestehen (siehe oben)
(define list-of
  (lambda (t)
    (signature (mixed empty-list
                      (pair-of t (list-of t))
                      ))))

; (b) rekursive Prozeduren
; i.) schreiben einer Prozedur "last"
;     - operiert auf einer beliebigen Liste
;     - soll nur letztes Element der Liste wiedergeben
;     - bei leerer Liste -> Fehlermeldung "Liste ist leer"
