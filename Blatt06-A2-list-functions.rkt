;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt06-A2-list-functions) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
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

(: last ((list-of %a) -> %a))

(check-error (last empty) "Liste ist leer")
(check-expect (last Liste123) 3)
(check-expect (last Wochenende) "Sonntag")
(check-expect (last (make-pair #t
                               empty)) #t)
(check-expect (last (make-pair -1
                               (make-pair 0
                                          (make-pair 1
                                                    empty)))) 1)

(define last
  (lambda (xs)
    (cond
      ((empty? xs) (violation "Liste ist leer"))
      ((empty? (rest xs)) (first xs))
      ((pair? xs) (last (rest xs))))))

; ii.) schreiben einer Prozedur "elem?"
;      überprüft, ob übergebene Zahl (n), Element der Liste ist

(: elem? (number (list-of number) -> boolean))

(check-expect (elem? 0 empty) #f)
(check-expect (elem? 1 (make-pair 1
                                 empty))  #t)
(check-expect (elem? -2 (make-pair 2
                                   (make-pair 3
                                              empty))) #f)

(define elem?
  (lambda (n xs)
    (cond
      ((empty? xs) #f)
      ((= n (first xs)) #t)
      ((pair? xs) (elem? n (rest xs))))))

; iii.)schreiben einer Prozedur "max-list"
;      - zurückgeben der größten Zahl in einer übergebenen Liste
;      - bei leerer Liste -> Fehlermeldung "Liste ist leer"

(: max-list ((list-of natural) -> natural))

(check-error (max-list empty) "Liste ist leer")
(check-expect (max-list (make-pair 1
                                   (make-pair 0
                                              empty))) 1)
(check-expect (max-list (make-pair 2
                                  (make-pair 42
                                             (make-pair 5
                                                        empty)))) 42)

(define max-list
  (lambda (xs)
    (cond
      ((empty? xs) (violation "Liste ist leer"))
      ((empty? (rest xs)) (first xs))
      ((and (pair? xs)
            (< (first xs) (first (rest xs)))) (max-list (rest xs)))
      (else (max-list (make-pair (first xs)
                                 (rest (rest xs))))))))

; iv.) schreiben einer Prozedur "init"
;       - gibt alle Elemnente, außer das letzte ELement, einer Liste an
;       - bei leerer Liste -> Fehlermeldung "Liste ist leer"

(: init ((list-of %a) -> (list-of %a)))

(check-error (init empty) "Liste ist leer")
(check-expect (init Liste123) (make-pair 1
                                         (make-pair 2
                                                    empty)))
(check-expect (init (make-pair #f
                               empty)) empty)
(check-expect (init (make-pair 0
                               (make-pair 12
                                          (make-pair 0
                                                     empty)))) (make-pair 0
                                                                          (make-pair 12
                                                                                     empty)))

(define init
  (lambda (xs)
    (cond
      ((empty? xs) (violation "Liste ist leer"))
      ((empty? (rest xs)) empty)
      (else (make-pair (first xs)
                       (init (rest xs)))))))

; v.) schreiben einer "all-equal?" Prozedur
;      - überprüft, ob alle Elemente einer Liste gleich sind
;      - bei einer leeren Liste soll #t ausgegeben werden

(: all-equal? ((list-of number) -> boolean))


