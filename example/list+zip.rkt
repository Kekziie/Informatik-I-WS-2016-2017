;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname list+zip) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; prelude.rkt

; a)
; akzeptiert Liste von Booleans
; wenn mind. ein Listenelement #t -> #t
; leere Liste -> #f
(: list-or ((list-of boolean) -> boolean))
(check-expect (list-or empty) #f)
(check-expect (list-or (list #t)) #t)
(check-expect (list-or (list #f)) #f)
(check-expect (list-or (list #t #f)) #t)
(check-expect (list-or (list #f #f #t)) #t)
(check-expect (list-or (list #f #f #f)) #f)
(define list-or
  (lambda (l)
    (cond
      ((empty? l) #f)
      ((pair? l) (or (first l)
                     (list-or (rest l)))))))

; b)
; akzeptiert Liste von Zahlen xs und ein Zahl x
; entfernt alle x aus der Liste xs
; Reihenfolge der übrigen Listen bleibt gleich
(: delete-all ((list-of number) number -> (list-of number)))
(check-expect (delete-all (list 1 3 2 3 5) 3) (list 1 2 5))
(check-expect (delete-all empty 2) empty)
(check-expect (delete-all (list 1 1 1) 1) empty)
(check-expect (delete-all (list 1 2) 2) (list 1))
(check-expect (delete-all (list 1 2 3 4 5 6) 3) (list 1 2 4 5 6))
(define delete-all
  (lambda (xs x)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (if (= x (first xs))
                      (delete-all (rest xs) x)
                      (make-pair (first xs) (delete-all (rest xs) x)))))))

; c)
; akzeptiertz eine Liste von Zahlen
; Duplikate aus einer Liste von Zahlen entfernt
; Reihenfolge der Elemente beibehalten
(: distinct ((list-of number) -> (list-of number)))
(check-expect (distinct (list 1 3 3 2 4 2 3)) (list 1 3 2 4))
(check-expect (distinct empty) empty)
(check-expect (distinct (list 1 2 3 4)) (list 1 2 3 4))
(check-expect (distinct (list 1 1 1 1)) (list 1))
(check-expect (distinct (list 1 2 3 1 2 3)) (list 1 2 3))
(define distinct
  (lambda (l)
    (cond
      ((empty? l) empty)
      ((pair? l) (make-pair (first l)
                            (distinct (delete-all (rest l)
                                                  (first l))))))))

;----------------------------------------------------------------------------
; selectionsort.rkt

; a)
; akzeptiert eine nicht-leere Liste von natürlichen Zahlen
; übergibt die kleinste Zahl in der Liste
(: minimum ((list-of natural) -> natural))
(check-expect (minimum (list 3 1 2 4 2 3)) 1)
(check-expect (minimum (list 1 2 11 1 1)) 1)
(check-error (minimum empty) "list empty")
(check-expect (minimum (list 42)) 42)
(check-expect (minimum (list 3 4 5 6 7)) 3)
(define minimum
  (lambda (l)
    (cond
      ((empty? l) (violation "list empty"))
      ((pair? l) (minimum-worker (first l) (rest l))))))

(define minimum-worker
  (lambda (x xs)
    (cond
      ((empty? xs) x)
      ((pair? xs) (minimum-worker (min x (first xs)) (rest xs))))))

; b)
; akzeptiert eine natürliche Zahl x und eine Liste von natürlichen Zahlen xs
; erstes Vorkommen von n aus der Liste entfernen
; Reihenfolge beibehalten
(: delete (natural (list-of natural) -> (list-of natural)))
(check-expect (delete 2 (list 1 2 4 2 3)) (list 1 4 2 3))
(check-expect (delete 2 empty) empty)
(check-expect (delete 1 (list 1)) empty)
(check-expect (delete 1 (list 1 1 1)) (list 1 1))
(check-expect (delete 5 (list 1 2 3 4 5)) (list 1 2 3 4))
(check-expect (delete 1 (list 2 2)) (list 2 2))
(define delete
  (lambda (x xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (if (= x (first xs))
                      (rest xs)
                      (make-pair (first xs) (delete x (rest xs))))))))

; c)
; akzptiert Liste von natürlichen Zahlen
; sortiert eine Liste
(: selection-sort ((list-of natural) -> (list-of natural)))
(check-expect (selection-sort empty) empty)
(check-expect (selection-sort (list 1)) (list 1))
(check-expect (selection-sort (list 1 2)) (list 1 2))
(check-expect (selection-sort (list 3 2 1)) (list 1 2 3))
(check-expect (selection-sort (list 2 5 2 6 1)) (list 1 2 2 5 6))
(define selection-sort
  (lambda (l)
    (cond
      ((empty? l) empty)
      ((pair? l)
       (let ((m (minimum l)))
             (make-pair m (selection-sort (delete m l))))))))

;----------------------------------------------------------------------------
; prefixsuffix.rkt

; a)
; akzeptiert 2 Listen von Strings xs und ys
; überprüft ob alle Elemente aus xs in derselben Reihenfolge am Anfang von ys
; falls xs leere Liste -> #f
(: is-prefix? ((list-of string) (list-of string) -> boolean))
(check-expect (is-prefix? empty (list "1" "2" "3" "4")) #t)
(check-expect (is-prefix? empty empty) #t)
(check-expect (is-prefix? (list "a") empty) #f)
(check-expect (is-prefix? (list "a") (list "a")) #t)
(check-expect (is-prefix? (list "me") (list "me" "em")) #t)
(check-expect (is-prefix? (list "b" "c") (list "c" "b")) #f)
(check-expect (is-prefix? (list "a" "b" "c") (list "a" "b" "c" "d")) #t)
(check-expect (is-prefix? (list "a" "c") (list "b" "a" "c")) #f)
(define is-prefix?
  (lambda (xs ys)
    (cond
      ((empty? xs) #t)
      ((empty? ys) #f)
      ((pair? xs) (if (string=? (first xs) (first ys))
                      (is-prefix? (rest xs) (rest ys))
                      #f)))))

; b)
; akzeptiert 2 Listen von Strings xs und ys
; überprüft ob alle Elemente aus xs in derselben Reihenfolge am Ende von ys
; wenn xs leere liste -> #t
(: is-suffix? ((list-of string) (list-of string) -> boolean))
(check-expect (is-suffix? empty empty) #t)
(check-expect (is-suffix? empty (list "a")) #t)
(check-expect (is-suffix? (list "a") empty) #f)
(check-expect (is-suffix? (list "a") (list "a")) #t)
(check-expect (is-suffix? (list "1" "2") (list "a" "1" "2")) #t)
(check-expect (is-suffix? (list "3") (list "1" "3" "1")) #f)
(define is-suffix?
  (lambda (xs ys)
    (is-prefix? (reverse xs) (reverse ys))))

; c)
; akzeptiert eine Liste von Strings xs und ein String s
; sagt aus, an welchen Stellen der String vorkommt
; Index beginnt bei 0
(: elem-indices (string (list-of string) -> (list-of natural)))
(check-expect (elem-indices "a" (list "a" "b" "a")) (list 0 2))
(check-expect (elem-indices "a" empty) empty)
(check-expect (elem-indices "a" (list "1" "2" "3")) empty)
(define elem-indices
  (lambda (s xs)
    (elem-indices-worker s 0 xs)))

(: elem-indices-worker (string natural (list-of string) -> (list-of natural)))
(check-expect (elem-indices-worker "a" 0 (list "a" "b" "a")) (list 0 2))
(define elem-indices-worker
  (lambda (s n xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (if (string=? s (first xs))
                      (make-pair n (elem-indices-worker s (+ n 1) (rest xs)))
                      (elem-indices-worker s (+ n 1) (rest xs)))))))

;----------------------------------------------------------------------------
; zip.rkt
; Tupel
(: make-tuple (%a %b -> (tuple-of %a %b)))
(define-record-procedures-parametric tuple tuple-of
  make-tuple
  tuple?
  (tuple-first
   tuple-second))

; akzeptiert 2 Listen xs und ys und gibt Liste von Tupeln zurück
; erstes Tupel enthält jeweils erstes Element von xs und ys usw.
; wenn eine Liste leer, werden die restlichen Elemente der anderen Liste verworfen
(: zip ((list-of %a) (list-of %b) -> (list-of (tuple-of %a %b))))
(check-expect (zip (list 1 2) (list 3 4)) (list (make-tuple 1 3) (make-tuple 2 4)))
(check-expect (zip empty empty) empty)
(check-expect (zip empty (list 1)) empty)
(check-expect (zip (list 1) empty) empty)
(check-expect (zip (list 1) (list 1 2)) (list (make-tuple 1 1)))
(check-expect (zip (list "a" "b") (list "c")) (list (make-tuple "a" "c")))
(define zip
  (lambda (xs ys)
    (cond
      ((empty? xs) empty)
      ((empty? ys) empty)
      ((and (pair? xs) (pair? ys)) (make-pair (make-tuple (first xs) (first ys))
                                              (zip (rest xs) (rest ys)))))))