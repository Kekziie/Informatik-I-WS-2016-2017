;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt06-A3-insertion-sort) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 3

; Daten- und Recorddefinition für polymorphe Paare
; ein polymorphes Paar (pair) besteht aus
; - erste Komponente (first)
; - zweite Komponente (rest)
; Hinweis: Komponenten können beliebige Werte annehmen
(define-record-procedures-parametric pair pair-of
  make-pair
  pair?
  (first
   rest))

; Datendefinitionen
(define Liste123
  (make-pair 1
             (make-pair 2
                        (make-pair 3
                                   empty))))
(define Liste1
  (make-pair 3
             (make-pair 12
                        (make-pair 45
                                   (make-pair 113
                                              empty)))))

; Signatur von list-of
; eine Liste kann:
;  - leer sein (empty-list)
;  - aus einem Paar (pair-of) bestehen (siehe oben)
(define list-of
  (lambda (t)
    (signature (mixed empty-list
                      (pair-of t (list-of t))
                      ))))

; (a) schreiben einer Prozedur "insert-sorted"
;      - bei Eingabe einer reelen Zahl, soll diese, an die richtige Stelle einer Liste einsortiert werden

(: insert-sorted (real (list-of real) -> (list-of real)))

(check-expect (insert-sorted 1 empty) (make-pair 1
                                                 empty))
(check-expect (insert-sorted 0 (make-pair 42
                                          empty)) (make-pair 0
                                                             (make-pair 42
                                                                        empty)))
(check-expect (insert-sorted 3 (make-pair -3
                                          empty)) (make-pair -3
                                                             (make-pair 3
                                                                        empty)))                                         
(check-expect (insert-sorted -2 Liste123) (make-pair -2
                                                     (make-pair 1
                                                                (make-pair 2
                                                                           (make-pair 3
                                                                                      empty)))))
(check-expect (insert-sorted 23 Liste1) (make-pair 3
                                                   (make-pair 12
                                                              (make-pair 23
                                                                         (make-pair 45
                                                                                    (make-pair 113
                                                                                               empty))))))

(define insert-sorted
  (lambda (n xs)
    (cond
      ((empty? xs) (make-pair n
                              empty))
      ((empty? (rest xs)) (if (>= n (first xs))
                              (make-pair (first xs)
                                         (make-pair n
                                                    empty))
                              (make-pair n
                                         (make-pair (first xs)
                                                    empty))))
      (else (if (< n (first xs))
                (make-pair n
                           (make-pair (first xs)
                                      (rest xs)))
                (make-pair (first xs)
                           (insert-sorted n (rest xs))))))))

