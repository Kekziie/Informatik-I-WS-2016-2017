;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise12-cycle) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Übung 12

; Prozedur last
; - akzpetiert eine nicht-leere Liste xs
; - gibt letztes Listenelement zurück
(: last ((list-of %a) -> %a))
(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list "a")) "a")
(check-expect (last (list #t #f #t)) #t)
(define last
  (lambda (xs)
     (cond
       ((empty? (rest xs)) (first xs))
       ((pair? xs) (last (rest xs))))))

; Prozedur without-last
; - akzeptiert eine nicht-leere Liste xs
; - gibt die Liste zurück ohne letztes Element
(: without-last ((list-of %a) -> (list-of %a)))
(check-expect (without-last (list 1 2 3)) (list 1 2))
(check-expect (without-last (list "a")) empty)
(check-expect (without-last (list #t #f #t)) (list #t #f))
(define without-last
  (lambda (xs)
    (cond
      ((empty? (rest xs)) empty)
      ((pair? xs) (make-pair (first xs)
                             (without-last (rest xs)))))))

; Prozedur cycle
; - akzeptiert eine natürliche Zahl n und eine beliebige Liste xs
; - setzt n Elemente von Ende der Liste an den Anfang
(: cycle (natural (list-of %a) -> (list-of %a)))
(check-expect (cycle 4 (list "a" "b")) (list "a" "b"))
(check-expect (cycle 0 (list 1 2 3)) (list 1 2 3))
(check-expect (cycle 1 empty) empty)
(check-expect (cycle 1 (list 1 2 3)) (list 3 1 2))
(check-expect (cycle 5 (list 1 2 3 4 5 6 7 8 9)) (list 5 6 7 8 9 1 2 3 4))
(define cycle
  (lambda (n xs)
    (cond
      ((empty? xs) xs)
      ((= n 0) xs)
      ((pair? xs) (if (= n 1)
                      (make-pair (last xs)
                                 (without-last xs))
                      (cycle (- n 1) (make-pair (last xs)
                                                (without-last xs))))))))
