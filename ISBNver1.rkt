;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname ISBNver1) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; PÜ 06 ISBN
; a)
; Summe über die Elemente einer Liste xs
(: sum ((list-of number) -> number))

(check-expect (sum empty) 0)
(check-expect (sum (list 1)) 1)
(check-expect (sum (list 1 2)) 3)
(check-expect (sum (list -1 1)) 0)
(check-within (sum (list -0.5 1/2)) 0 0.01)

(define sum
  (lambda (xs)
    (cond
      ((empty? xs) 0)
      ((pair? xs) (+ (first xs)
                     (sum (rest xs)))))))

; b)
; Multipliziert ein Zahl x auf alle Elemente in xs
(: mult (number (list-of number) -> (list-of number)))

(check-expect (mult 0 empty) empty)
(check-expect (mult 0 (list 1 2 3)) (list 0 0 0))
(check-expect (mult 1 (list 2)) (list 2))
(check-within (mult -1 (list -1 -4 0.5)) (list 1 4 -0.5) 0.01)

(define mult
  (lambda (x xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (make-pair (* x (first xs))
                             (mult x (rest xs)))))))

; c)
; akzeptiert eine natürliche Zahl x und eine Liste xs
; soll die ersten n Elemente aus der Liste xs entfernen
; falls xs weniger ELemente als n -> empty
(: drop (natural (list-of %a) -> (list-of %a)))

(check-expect (drop 2 (list "a" "b" "c" "d")) (list "c" "d"))
(check-expect (drop 2 empty) empty)
(check-expect (drop 3 (list 1)) empty)
(check-expect (drop 1 (list 3 2 1)) (list 2 1))
(check-expect (drop 0 (list 1)) (list 1))

(define drop
  (lambda (x xs)
    (cond
      ((empty? xs) empty)
      ((= x 0) xs)
      ((pair? xs) (drop (- x 1) (rest xs))))))

; d)
; liefert die Elemente einer Liste xs an jeder n-ten Stelle
(: every-nth (natural (list-of %a) -> (list-of %a)))

(check-expect (every-nth 1 (list 1 2 3)) (list 1 2 3))
(check-expect (every-nth 0 empty) empty)
(check-expect (every-nth 2 (list 1 2 3 4 5)) (list 1 3 5))
(check-expect (every-nth 42 (list 2)) (list 2))
(check-expect (every-nth 3 (list 1 2 3 4 5 6 7 8 9)) (list 1 4 7))

(define every-nth
  (lambda (n xs)
    (cond
      ((empty? xs) empty)
      ((= n 1) xs)
      ((pair? xs) (make-pair (first xs)
                             (every-nth n (drop n xs)))))))

; e)
; Liste soll genaue 13 Elemente der Signatur t liefern
(define list-of-13
  (lambda (t)
    (signature (combined (list-of t)
                         (predicate (lambda (xs)
                                      (= (length xs) 13)))))))

; f)
; berechnet modulo 10
(: check-number (natural -> natural))
(check-expect (check-number 1) 1)
(check-expect (check-number 10) 0)
(check-expect (check-number 5) 5)
(check-expect (check-number 20) 0)
(define check-number
  (lambda (n)
    (modulo n 10)))

; ISBN Nummern
(: ISBN1 (list-of-13 natural))
(define ISBN1 (list 9 7 8 3 7 6 5 7 2 7 8 1 8)) ; richtig
(: ISBN2 (list-of-13 natural))
(define ISBN2 (list 9 7 8 3 7 6 5 7 2 7 8 1 7)) ; falsch
(: ISBN3 (list-of-13 natural))
(define ISBN3 (list 9 7 8 3 8 3 5 1 0 1 5 5 5)) ; richtig
(: ISBN4 (list-of-13 natural))
(define ISBN4 (list 9 7 8 0 3 4 5 3 4 1 4 6 4)) ; richtig
(: ISBN5 (list-of-13 natural))
(define ISBN5 (list 9 7 8 3 5 2 8 6 6 4 4 2 8)) ; richtig

; checkt eine liste aus 13 natürlichen Zahlen (ISBN)
(: check-isbn ((list-of-13 natural) -> boolean))
(check-expect (check-isbn ISBN1) #t)
(check-expect (check-isbn ISBN2) #f)
(check-expect (check-isbn ISBN3) #t)
(check-expect (check-isbn ISBN4) #t)
(check-expect (check-isbn ISBN5) #t)

(define check-isbn
  (lambda (xs)
    (zero? (check-number (+ (sum (mult 3 (every-nth 2 (rest xs))))
                            (sum (every-nth 2 xs)))))))