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

(define foldl
  (lambda (z c xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (foldl (c z (first xs)) c (rest xs))))))