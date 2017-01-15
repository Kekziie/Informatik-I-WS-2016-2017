;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt09-A1-mountain-peaks) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1

; Hilfsprozeduren
; unlines und print gegeben in Aufgabenstellung:
(: unlines ((list-of string) -> string))
(define unlines
  (lambda (ys)
    (fold ""
          (lambda (x xs)
            (string-append x "\n" xs))
          ys)))

(: print ((list-of string) -> %nothing))
(define print
  (lambda (ss)
    (write-string (unlines ss))))

; Prozedur "Whitespace"
; akzeptiert natürliche Zahl n
; gibt n Whitespaces in einem String aus

(: whitespace (natural -> string))
(check-expect (whitespace 0) "")
(check-expect (whitespace 1) " ")
(check-expect (whitespace 2) "  ")
(define whitespace
  (lambda (n)
    (cond
      ((= n 0) "")
      ((> n 0) (string-append " " (whitespace (- n 1)))))))

; Prozedur "Whitespace-string"
; - akzeptiert eine Liste von Strings
; - fügt ein oder mehrere Whitespaces an String je nach Position und Länge einer Liste
; (-> hilft bei "Einrückung" der Zeilen des Gebirges)

(: whitespace-string ((list-of string) -> (list-of string)))
(check-expect (whitespace-string (list "a" "b" "c")) (list "  a" " b" "c"))
(check-expect (whitespace-string empty) empty)
(define whitespace-string
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (make-pair (string-append (whitespace (- (length xs) 1)) (first xs))
                             (whitespace-string (rest xs)))))))

; (print (mountaint-peaks n)) zeichnet "Gebirgszug" für jede natürlich Zahl n
; (vgl. Zeichnungen)
;                                                          /\          
;                                          /\             / /\
;                            /\           /\ \           / /\ \    
;                /\         / /\         / /\ \         / / /\ \
; n=1: /\  n=2: /\ \  n=3: / /\ \  n=4: / /\ \ \  n=5: / / /\ \ \