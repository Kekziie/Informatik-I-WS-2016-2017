;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise7-fold) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Übung 7 fold "Listenfaltung"

; i) foldr(ight)
; Listenfaltung rechts
(: foldr (%b (%a %b -> %b) (list-of %a) -> %b))

(check-expect (foldr empty make-pair (list 1 2 3)) (list 1 2 3))
(check-expect (foldr 0 + (list 1 2 3 4 5)) 15)
(check-expect (foldr 1 * (list -1 1 0)) 0)
(check-expect (foldr 5 * (list 1 2 3)) 30)
(check-expect (foldr 0 - (list -1 1)) -2)
(check-expect (foldr 1 = empty) 1)

(define foldr
  (lambda (z c xs)
    (cond
      ((empty? xs) z)
      ((pair? xs) (c (first xs)
                     (foldr z c (rest xs)))))))

; ii) foldl(eft)
; Listenfaltung links
(: foldl (%b (%b %a -> %b) (list-of %a) -> %b))

(check-expect (foldl 1 * (list 1 2 3)) 6)
(check-expect (foldl 0 * empty) 0)
(check-expect (foldl 0 + (list 1 2 3 4 5)) 15)
(check-expect (foldl 2 - (list 0 2 4)) -4)

(define foldl
  (lambda (z c xs)
    (cond
      ((empty? xs) z)
      ((pair? xs) (foldl (c z (first xs)) c (rest xs))))))

; foldl ist endrekursiv, da z als Akkumulator fungiert

; iii) my-reverse <=> reverse
; soll Liste verkehrt herum wiedergeben
(: my-reverse ((list-of %a) -> (list-of %a)))

(check-expect (my-reverse (list 1 2 3)) (list 3 2 1))
(check-expect (my-reverse empty) empty)
(check-expect (my-reverse (list "a")) (list "a"))
(check-expect (my-reverse (list #t #f 1 2 3)) (list 3 2 1 #f #t))
(check-expect (my-reverse (list 1 2 3 4 5)) (list 5 4 3 2 1))

(define my-reverse
  (lambda (xs)
    (foldl empty (lambda (a b)
                   (make-pair b a)) xs)))

; iv) contains?
; - akzeptiert Element x, Prozedur und beliebige Liste xs
; - gibt an, ob Element x in Liste xs
(: contains? (%a (%a %a -> boolean) (list-of %a) -> boolean))

(check-expect (contains? "a" string=? (list "a" "b")) #t)
(check-expect (contains? 1 = (list 5 4 3 2 1)) #t)
(check-expect (contains? 3 = (list 42 0 -1 -2 -3)) #f)
(check-expect (contains? "BAUM" string=? (list "baum")) #f)
(check-expect (contains? 2 = empty) #f)

(define contains?
  (lambda (z c xs)
    (fold #f (lambda (a b)
               (or (c a z) b)) xs)))

; v) multi-comp(arator)
; - akzeptiert Element x, Prozedur und beliebige Liste xs
; - vergleicht Element x mit jedem Element der Liste xs
(: multi-comp (%a (%a %a -> boolean) (list-of %a) -> boolean))

(check-expect (multi-comp 2 = (list 2 2 2)) #t)

(define multi-comp
  (lambda (z c xs)
    (cond
      ((empty? xs) #f)
      ((pair? xs) (fold #t (lambda (a b)
                             (and (c a z) b)) xs)))))