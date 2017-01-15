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

; (print (mountaint-peaks n)) zeichnet "Gebirgszug" für jede natürlich Zahl n
; (vgl. Zeichnungen)
;                                                          /\          
;                                          /\             / /\
;                            /\           /\ \           / /\ \    
;                /\         / /\         / /\ \         / / /\ \
; n=1: /\  n=2: /\ \  n=3: / /\ \  n=4: / /\ \ \  n=5: / / /\ \ \