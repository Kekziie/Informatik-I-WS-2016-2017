;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname words) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm") (lib "starwars.rkt" "installed-teachpacks"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm") (lib "starwars.rkt" "installed-teachpacks")))))
; PÜ 07 words
; Eingebaute Prozeduren:
; (: string->strings-list (string -> (list-of string)))
; (: strings-list->string ((list-of string) -> string))
; Parametrisch-polymorphe 2-Tupel
(define-record-procedures-parametric
tuple
tuple-of
make-tuple
tuple?
(tuple-first tuple-second))
; Prüft, ob ein String whitespace ist
(: whitespace? (string -> boolean))
(check-expect (whitespace? " ") #t)
(check-expect (whitespace? "\n") #t)
(check-expect (whitespace? "\t") #t)
(check-expect (whitespace? "") #f)
(check-expect (whitespace? "a") #f)
(define whitespace?
(lambda (c)
(or (string=? c "\n")
(string=? c "\t")
(string=? c " "))))
; Gibt eine Liste zurück, die alle Elemente der möglicherweise verschachtelten
; Eingabelisten in einer "Ebene" enthält
(: concat ((list-of (list-of %a)) -> (list-of %a)))
(check-expect (concat (list (list 1 2 3 4) (list 42 43) (list 567))) (list 1 2 3 4 42 43 567))
(check-expect (concat (list (list 1 2 3 4) empty)) (list 1 2 3 4))
(define concat
(lambda (xss)
(cond
 ((empty? xss) empty)
 ((pair? xss) (append (first xss) (concat (rest xss)))))))
;---------------------------------------------------------
; a)
; enfernt sämtlichen whitespace am Anfang einer Zeichenliste
(: eat-leading-whitespace ((list-of string) -> (list-of string)))

(check-expect (eat-leading-whitespace (list " " "f" "o" "o"))
              (list "f" "o" "o"))
(check-expect (eat-leading-whitespace empty) empty)
(check-expect (eat-leading-whitespace (list "\n" " " "x")) (list "x"))
(check-expect (eat-leading-whitespace (list " ")) empty)
(check-expect (eat-leading-whitespace (string->string-list "  \t \n apple"))
              (string-list->string "apple"))
(check-expect (eat-leading-whitespace (list "x" " " "y")) (list "x" " " "y"))

(define eat-leading-whitespace
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (if (whitespace? (first xs))
                      (eat-leading-whitespace (rest xs))
                      xs)))))

