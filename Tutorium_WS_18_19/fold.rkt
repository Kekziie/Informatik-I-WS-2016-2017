;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname fold) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
; a)
; fold
; Listenfaltung von links
(: foldl (%b (%b %a -> %b) (list-of %a) -> %b))

(check-expect (foldl 0 + (list 40 2)) 42)
(check-expect (foldl 1 * (list 2 3)) 6)
(check-expect (foldl 0 + empty) 0)

(define foldl
  (lambda (z c xs)
    (match xs
      (empty z)
      ((make-pair y ys) (foldl (c z y) c ys)))))

; b)
; gibt eine Liste in umgekehrter Reihenfolge wieder
(: my-reverse ((list-of %a) -> (list-of %a)))

(check-expect (my-reverse (list 1 2 3)) (list 3 2 1))
(check-expect (my-reverse empty) empty)
(check-expect (my-reverse (list 2 4 6)) (list 6 4 2))
(check-expect (my-reverse (list "a")) (list "a"))

(define my-reverse
  (lambda (xs)
    (foldl empty (lambda (acc x)
                   (make-pair x acc)) xs)))

;-------------
; Gibt nur die Elemente der Eingabeliste zurück, für die das Prädikat #t ist
(: filter ((%a -> boolean) (list-of %a) -> (list-of %a)))
(check-expect (filter (lambda (x) (= (modulo x 2) 0)) (list 1 2 3 4 5)) (list 2 4))
(define filter
  (lambda (p xs)
   (let ((aux (lambda (x xs)
                     (if (p x)
                        (make-pair x xs)
                         xs))))
     (fold empty aux xs))))

; a)
; überprüft für ein Element x, ob es in einer Liste xs vorkommt
(: contains? (%a (%a %a -> boolean) (list-of %a) -> boolean))

(check-expect (contains? "bar" string=? (list "foo" "bar" "baz")) #t)
(check-expect (contains? 1 = (list 1 1 1 1 1)) #t)
(check-expect (contains? #f boolean=? (list #f)) #t)
(check-expect (contains? 0 = (list 1 2 3)) #f)
(check-expect (contains? 2 = empty) #f)

(define contains?
  (lambda (x f xs)
    (fold #f (lambda (y acc)
               (or (f x y) acc)) xs)))

; b)
; löscht Duplikate aus einer Liste
(: distinct ((%a %a -> boolean) (list-of %a) -> (list-of %a)))

(check-expect (distinct = (list 1 2 2 3 4 1)) (list 2 3 4 1))
(check-expect (distinct = (list 1 2 3)) (list 1 2 3))
(check-expect (distinct = empty) empty)
(check-expect (distinct string=? (list "a" "a")) (list "a"))

(define distinct
  (lambda (f xs)
    (fold empty (lambda (x acc)
                  (if (contains? x f acc)
                      acc
                      (make-pair x acc))) xs)))

; c)
; fasst Werte einer Liste in Gruppen zusammen, die in einem Kriterium übereinstimmen
(: groupby ((%a -> %b) (%b %b -> boolean) (list-of %a) -> (list-of (list-of %a))))

(check-expect (groupby (lambda (n) (modulo n 3)) = (list 1 2 3 4 5 6))
              (list (list 1 4) (list 2 5) (list 3 6)))
(check-expect (groupby (lambda (x) (= x 0)) boolean=? empty) empty)
(check-expect (groupby (lambda (x) (< x 0)) boolean=? (list -1 2 1 -2))
              (list (list 2 1) (list -1 -2)))

 (define groupby
   (lambda (f eq? xs)
     (map (lambda (x)
            (filter (lambda (k)
                      (eq? (f k) x)) xs)) (distinct eq? (map f xs)))))

(: groupby2 ((%a -> %b) (%b %b -> boolean) (list-of %a) -> (list-of (list-of %a))))

(check-expect (groupby2 (lambda (n) (modulo n 3)) = (list 1 2 3 4 5 6))
              (list (list 1 4) (list 2 5) (list 3 6)))
(check-expect (groupby2 (lambda (x) (= x 0)) boolean=? empty) empty)
(check-expect (groupby2 (lambda (x) (< x 0)) boolean=? (list -1 2 1 -2))
              (list (list 2 1) (list -1 -2)))

(define groupby2
   (lambda (f eq? xs)
     (let ((groups-maker (lambda (x)
                           (filter (lambda (k)
                               (eq? (f k) x)) xs)))
           (liste (distinct eq? (map f xs))))
       (map groups-maker liste))))
     