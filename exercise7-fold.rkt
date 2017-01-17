;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname exercise7-fold) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Übung 7 fold "Listenfaltung"

; i) foldr(ight)
; Listenfaltung rechts
(: foldr (%b (%a %b -> %b) (list-of %a) -> %b))

(define foldr
  (lambda (z c xs)
    (cond
      ((empty? xs) z)
      ((pair? xs) (c (first xs)
                     (foldr z c (rest xs)))))))