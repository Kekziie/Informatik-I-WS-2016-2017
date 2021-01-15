;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname words) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
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

(: concat2 ((list-of (list-of %a)) -> (list-of %a)))
(check-expect (concat2 (list (list 1 2 3 4) (list 42 43) (list 567))) (list 1 2 3 4 42 43 567))
(check-expect (concat2 (list (list 1 2 3 4) empty)) (list 1 2 3 4))
(define concat2
(lambda (xss)
(match xss
(empty empty)
((make-pair y ys) (append y (concat2 ys))))))
;---------------------------------------------------------
; a)
; enfernt sämtlichen whitespace am Anfang einer Zeichenliste
(: eat-leading-whitespace ((list-of string) -> (list-of string)))

(check-expect (eat-leading-whitespace (list " " "f" "o" "o"))
              (list "f" "o" "o"))
(check-expect (eat-leading-whitespace empty) empty)
(check-expect (eat-leading-whitespace (list "\n" " " "x")) (list "x"))
(check-expect (eat-leading-whitespace (list " ")) empty)
(check-expect (eat-leading-whitespace (string->strings-list "  \t \n apple"))
              (list "a" "p" "p" "l" "e"))
(check-expect (eat-leading-whitespace (list "x" " " "y")) (list "x" " " "y"))

(define eat-leading-whitespace
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (if (whitespace? (first xs))
                      (eat-leading-whitespace (rest xs))
                      xs)))))

; enfernt sämtlichen whitespace am Ende einer Zeichenliste
(: eat-leading-whitespace2 ((list-of string) -> (list-of string)))

(check-expect (eat-leading-whitespace2 (list "f" "o" "o" " "))
              (list "f" "o" "o"))
(check-expect (eat-leading-whitespace2 empty) empty)
(check-expect (eat-leading-whitespace2 (list "x" "\n" " ")) (list "x"))
(check-expect (eat-leading-whitespace2 (list " ")) empty)
(check-expect (eat-leading-whitespace2 (string->strings-list "apple   \n"))
              (list "a" "p" "p" "l" "e"))
(check-expect (eat-leading-whitespace2 (list "x" " " "y")) (list "x" " " "y"))

(define eat-leading-whitespace2
  (lambda (xs)
    (reverse (eat-leading-whitespace (reverse xs)))))

; b)
; packt das erste Wort, also alle Strings bis whitspace, in 1.Tupel
; whitspace + Rest in 2.Tupel
(: next-word ((list-of string) -> (tuple-of (list-of string) (list-of string))))

(check-expect (next-word (list "f" "o" "o" " " "b" "a" "r"))
              (make-tuple (list "f" "o" "o")
                          (list " " "b" "a" "r")))
(check-expect (next-word (list " " "x" " " "y"))
              (make-tuple (list "x")
                          (list " " "y")))
(check-expect (next-word empty)
              (make-tuple empty empty))
(check-expect (next-word (string->strings-list "       x"))
              (make-tuple (list "x") empty))
(check-expect (next-word (string->strings-list "       x  "))
              (make-tuple (list "x") (list " " " ")))

(define next-word
  (lambda (xs)
    (next-word-worker (eat-leading-whitespace xs) empty)))

(: next-word-worker ((list-of string) (list-of string)
                                      -> (tuple-of (list-of string) (list-of string))))

(define next-word-worker
  (lambda (xs acc)
    (match xs
      (empty (make-tuple acc empty))
      ((make-pair y ys) (if (whitespace? y)
                            (make-tuple acc xs)
                            (next-word-worker ys (append acc (list y))))))))

; c)
; zerlegt die Zeichenliste in eine Liste von Wörtern
(: words ((list-of string) -> (list-of (list-of string))))

(check-expect (words (string->strings-list "ab cd ef")) (list (list "a" "b")
                                                              (list "c" "d")
                                                              (list "e" "f")))
(check-expect (words empty) empty)
(check-expect (words (list "1")) (list (list "1")))
(check-expect (words (string->strings-list "x   \n   y")) (list (list "x")
                                                                (list "y")))
(define words
  (lambda (xs)
    (words-worker xs empty)))

(: words-worker ((list-of string) (list-of (list-of string))
                                  -> (list-of (list-of string))))

(define words-worker
  (lambda (xs acc)
    (match xs
      (empty acc)
      ((make-pair y ys)
       (let ((tupleword (next-word xs)))
             (words-worker (tuple-second tupleword)
                           (append acc (list (tuple-first tupleword)))))))))

; d)
; fügt zwischen jedes Paar den Wert x auf
(: intersperse (%a (list-of %a) -> (list-of %a)))

(check-expect (intersperse 1 (list 1 2)) (list 1 1 2))
(check-expect (intersperse "x" empty) empty)
(check-expect (intersperse #f (list #t)) (list #t))
(check-expect (intersperse 42 (list 1 2 3)) (list 1 42 2 42 3))

(define intersperse
  (lambda (x xs)
    (match xs
      (empty empty)
      ((make-pair _ empty) xs)
      ((make-pair y ys) (append (list y x) (intersperse x ys))))))

; e)
; trennt jedes Wort mit einem Komma
(: insert-commas (string -> string))

(check-expect (insert-commas "foo     bar \n  baz") "foo,bar,baz")
(check-expect (insert-commas " ") "")
(check-expect (insert-commas "    x    y") "x,y")
(check-expect (insert-commas "   apple") "apple")
(check-expect (insert-commas " x ") "x")

(define insert-commas
  (lambda (s)
    (strings-list->string
           (concat
              (intersperse (list ",")
                           (words (eat-leading-whitespace2 (string->strings-list s))))))))
