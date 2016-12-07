;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt07-A1-split-weave) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1

; (a)
; schreiben einer parametrisch polymorphen Record-Typ für tuple

; ein Tupel besteht aus
; - erstes Element (tuple-first)
; - zweites Element (tuple-second)

(: make-tuple (%a %b -> (tuple-of %a %b)))
(: tuple-first ((tuple-of %a %b) -> %a))
(: tuple-second ((tuple-of %a %b) -> %b))
(: tuple? (any -> boolean))

(define-record-procedures-parametric tuple tuple-of
  make-tuple
  tuple?
  (tuple-first
   tuple-second))

; Prozedur "split-list" soll eine Liste in zwei Listen aufspalten
; die Elemente sollen abwechselnd auf beide Ergebnislisten aufgeteilt werden
; bei ungerader Azahl in Eingabeliste -> 1. Ergebnisliste länger

(: split-list ((list-of %a) -> (tuple-of (list-of %a) (list-of %a))))

(check-expect (split-list (list 1 2 3 4 5)) (make-tuple (list 1 3 5)
                                                        (list 2 4)))
(check-expect (split-list (list "Hund" "Tiger" "Hamster" "Giraffe" "Katze" "Affe" "Meerschweinchen")) (make-tuple (list "Hund" "Hamster" "Katze" "Meerschweinchen")
                                                                                                                  (list "Tiger" "Giraffe" "Affe")))
(check-expect (split-list empty) (make-tuple empty
                                             empty))
(check-expect (split-list (list 3)) (make-tuple (list 3)
                                                empty))
(check-expect (split-list (list "Samstag" "Sonntag")) (make-tuple (list "Samstag")
                                                                  (list "Sonntag")))

(define split-list
  (lambda (xs)
    (cond
      ((empty? xs) (make-tuple empty
                               empty))
      ((empty? (rest xs)) (make-tuple (make-pair (first xs)
                                                empty)
                                      empty))
      ((and (pair? xs)
            (empty? (rest (rest xs)))) (make-tuple (make-pair (first xs)
                                                              empty)
                                                   (make-pair (first (rest xs))
                                                              empty)))
      (else (make-tuple (make-pair (first xs)
                                   (tuple-first (split-list (rest (rest xs)))))
                        (make-pair (first (rest xs))
                                   (tuple-second (split-list (rest (rest xs))))))))))
   
                                   

; (b)

; Prozedur "weave-lists" soll zwei Listen in eine Liste verschmelzen
; Ergebnisliste enthält Elemente der konsumierten Liste in abwechselnder Reihenfolge
; bei ungleicher Länge der Listen -> überschüssige Elemente werden an Ergebnisliste hinten angehängt

(: weave-lists ((tuple-of (list-of %a) (list-of %a)) -> (list-of %a)))

(check-expect (weave-lists (make-tuple (list 1 3 5 6)
                                       (list 2 4))) (list 1 2 3 4 5 6))
(check-expect (weave-lists (make-tuple empty
                                       empty)) empty)
(check-expect (weave-lists (make-tuple empty
                                       (list 1))) (list 1))
(check-expect (weave-lists (make-tuple (list 2 3)
                                       empty)) (list 2 3))
(check-expect (weave-lists (make-tuple (list 3 5)
                                       (list 2 4))) (list 3 2 5 4))

(define weave-lists
  (lambda (xs)
    (let ((t1 (tuple-first xs))
          (t2 (tuple-second xs)))
      (cond
        ((and (empty? t1)
              (empty? t2)) empty)
        ((and (empty? t1)
              (pair? t2)) t2)
        ((and (pair? t1)
              (empty? t2)) t1)
        (else (make-pair (first t1)
                         (make-pair (first t2)
                                    (weave-lists (make-tuple (rest t1)
                                                             (rest t2))))))))))
                                     
; (c)
; schreiben einer Hilfsprozedur list-equal? die 2 Listen vergleicht, ob sie identisch sind
(: list-equal? ((list-of %a) (list-of %a) -> boolean))

(check-expect (list-equal? empty empty) #t)
(check-expect (list-equal? empty (list 1)) #f)
(check-expect (list-equal? (list 3) empty) #f)
(check-expect (list-equal? (list 1) (list 1)) #t)
(check-expect (list-equal? (list 1 2 3) (list 3 2 1)) #f)
(check-expect (list-equal? (list 1 2 3) (list 1 2 3)) #t)

(define list-equal?
  (lambda (xs ys)
   (let ((empty-list1 (empty? xs))
         (empty-list2 (empty? ys)))
    (cond
      ((and empty-list1
            empty-list2) #t)
      ((and empty-list1
            (pair? ys)) #f)
      ((and (pair? xs)
            empty-list2) #f)
      ((and (= (first xs) (first ys))
            (list-equal? (rest xs) (rest ys))) #t)
      (else #f)))))
    

; schreiben eines check-property für:
; - die Prozeduren "split-list" und "weave-lists"
; - beliebige Listen xs mit natürlichen Zahlen
; soll folgende Äquivalenz prüfen: (weave-lists (split-list xs)) <-> xs

;(check-property
; (for-all
;     ((xs (list-of natural)))
;   (list-equal? (weave-lists (split-list xs)) xs)))
      
          


