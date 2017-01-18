;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt11-A3-powerset) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 3

; Potenzmenge P(S) einer Menge S ist die Menge aller Teilmengen von S
; P(S) enthält dabei immer die leere Menge Ø sowie S selbst
; z.B. P({1,2,3}) = {{1,2,3},{1,2},{2,3},{1,3},{1},{2},{3},Ø}

; Funktion "powerset" soll Potenzmenge einer Menge von beliebigen Elementen berechnen
; akzeptiert eine beliebige Liste xs und erstellt die Potenzmenge als Liste von Listen
(: powerset ((list-of %a) -> (list-of (list-of %a))))

(check-expect (powerset (list 1 2 3)) (list (list "Ø") (list 1 2 3) (list 1 2) (list 2 3) (list 1 3) (list 1) (list 2) (list 3)))
(check-expect (powerset (list #t #f)) (list (list "Ø") (list #t #f) (list #t) (list #f)))
(check-expect (powerset empty) (list (list "Ø")))

(define powerset
  (lambda (xs)
    (cond
      ((empty? xs) (make-pair (list "Ø")
                              empty))
      (else (append (powerset (rest xs))
                    (map (lambda (x)
                           (make-pair (first xs) x))
                         (powerset (rest xs)))
                    (list "Ø"))))))
      