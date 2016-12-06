;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt07-A2-luhn) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 2 Luhn-Check
; Verfahren definiert für einer Zahl n
; i) gehe die Ziffern von n, rechts nach links durch, verdopple jede 2.Ziffer
; ii) falls Verdopplung 2 Ziffern hat -> Aufspaltung in 2 unabhängig voneinander zu betrachtenden Ziffern
; iii) Summe aus allen Ziffern
; iv) dividiere durch 10
; v) Rest der Division 0 -> gültige Zahl n, Luhn-Check bestanden
;    Rest ungleich 0 -> ungültige Zahl n, Luhn-Check nicht bestanden

; (a)
; Prozedur "sum" soll Summe einer Liste von Zahlen berechnen

(: sum ((list-of number) -> number))

(check-error (sum empty) "Liste ist leer")
(check-expect (sum (list 1 2 3)) 6)
(check-expect (sum (list 0)) 0)
(check-expect (sum (list 2 -2 3)) 3)
(check-within (sum (list 1.5 2.5 -5 0)) -1 0.01)
(check-within (sum (list 2 1/2 -1)) 1.5 0.01)

(define sum
  (lambda (xs)
    (cond
      ((empty? xs) (violation "Liste ist leer"))
      ((empty? (rest xs)) (first xs))
      ((and (pair? xs)
            (empty? (rest (rest xs)))) (+ (first xs)
                                          (first (rest xs))))
      (else (+ (first xs)
               (sum (rest xs)))))))

; (b)
; Prozedur "digits" soll eine natürliche Zahl in ihre Ziffern zerlegen und als Liste zurückliefern
; Liste soll Ziffern von links nach rechts enthalten
; Hinweis: - benutzen von Prozeduren "quotient" und "remainder"
;          - dezimale Stellensystem (1024= 4x1 + 2x10 + 0x100 + 1x1000)

(: digits (natural -> (list-of natural)))

(check-expect (digits 123) (list 3 2 1))
(check-expect (digits 49) (list 9 4))
(check-expect (digits 1024) (list 4 2 0 1))
(check-expect (digits 0) (list 0))

(define digits
  (lambda (n) 
    (cond
      ((>= 9 n 0) (list n))
      ((>= 99 n 10) (list (remainder n 10) (quotient n 10)))
      ((>= 999 n 100) (list (remainder n 10) (remainder (quotient n 10) 10) (quotient n 100)))
      ((>= 9999 n 1000) (list (remainder n 10) (remainder (quotient n 10) 10) (remainder (quotient n 100) 10) (quotient n 1000))))))
    

; (c)
; Prozedur "double-every-other-number" soll:
; - Liste von Zahlen akzeptieren
; - jede 2.Zahl verdoppeln

(: double-every-other-number ((list-of number) -> (list-of number)))

(check-expect (double-every-other-number (list 1 2 3 4 5)) (list 1 4 3 8 5))
(check-expect (double-every-other-number (list 5 6)) (list 5 12))
(check-expect (double-every-other-number (list 1)) (list 1))
(check-expect (double-every-other-number empty) empty)

(define double-every-other-number
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((empty? (rest xs)) (list (first xs)))
      ((and (pair? xs)
            (empty? (rest (rest xs)))) (list (first xs)
                                             (* (first (rest xs)) 2)))
      (else (make-pair (first xs)
                       (make-pair (* (first (rest xs)) 2)
                                  (double-every-other-number (rest (rest xs)))))))))
 
; (d)
; Prozedur "map-digits" soll:
; - eine Liste von natürlichen Zahlen akzeptieren
; - jede Zahl in Liste ihrer Ziffern zerlegen (siehe Teilaufgabe 2b)
; - liefert Liste von Listen

(: map-digits ((list-of natural) -> (list-of (list-of natural))))

(check-expect (map-digits (list 2 13 9)) (list (list 2) (list 3 1) (list 9)))
(check-expect (map-digits empty) empty)
(check-expect (map-digits (list 1)) (list (list 1)))
(check-expect (map-digits (list 1 12)) (list (list 1) (list 2 1)))
(check-expect (map-digits (list 13)) (list (list 3 1)))
(check-expect (map-digits (list 12 3)) (list (list 2 1) (list 3)))

(define map-digits
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((empty? (rest xs)) (if (< (first xs) 10)
                              (list (list (first xs)))
                              (list (digits (first xs)))))
      ((pair? xs) (make-pair (if (< (first xs) 10)
                             (list (first xs))
                             (digits (first xs)))
                             (make-pair (if (< (first (rest xs)) 10)
                                            (list (first (rest xs)))
                                            (digits (first (rest xs))))
                                        (map-digits (rest (rest xs)))))))))
       
; (e)
; Prozedur "concat" soll
; - eine Liste von Listen akzeptieren
; - alle enthaltenen Listen aneinander hängen
; Hinweis: beachten von Prozedur "append"

(: concat ((list-of (list-of %a)) -> (list-of %a)))

(check-expect (concat empty) empty)
(check-expect (concat (list (list 1))) (list 1))
(check-expect (concat (list (list 2 1))) (list 2 1))
(check-expect (concat (list (list 3 1) (list 4 5 6))) (list 3 1 4 5 6))
(check-expect (concat (list (list 1 2) (list 3 4) (list 5))) (list 1 2 3 4 5))

(define concat
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((empty? (rest xs)) (first xs))
      ((pair? xs) (append (first xs)
                          (concat (rest xs)))))))
    
; (f)
; Prozedur "luhn-check" soll:
; - impletiert nach obigen Verfahren
; - #t bei bestandenem Check, #f sonst

(: luhn-check (natural -> boolean))

(check-expect (luhn-check 5678) #t)
(check-expect (luhn-check 6789) #f)

(define luhn-check
  (lambda (n)
    (= (modulo (sum (concat (map-digits (double-every-other-number (digits n))))) 10) 0)))
