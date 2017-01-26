;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt12-A2-btree-parser) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 2
; Definition Binärbaum

; Ein Knoten (node) besitzt
; - einen linken Zweig (left-branch),
; - eine Markierung (label) und
; - einen rechten Zweig (right-branch)
(: make-node (%a %b %c -> (node-of %a %b %c)))
(: node-left-branch  ((node-of %a %b %c) -> %a))
(: node-label        ((node-of %a %b %c) -> %b))
(: node-right-branch ((node-of %a %b %c) -> %c))
(define-record-procedures-parametric node node-of
  make-node
  node?
  (node-left-branch
   node-label
   node-right-branch))

; Der leere Baum (the-empty-tree) besitzt keine weiteren Eigenschaften
(: make-empty-tree (-> the-empty-tree))
(define-record-procedures the-empty-tree
  make-empty-tree
  empty-tree?
  ())

; Der leere Baum 
(: empty-tree the-empty-tree)
(define empty-tree (make-empty-tree))

; Signatur für Binärbäume (btree-of t) mit Markierungen der Signatur t
; (im linken/rechten Zweig jedes Knotens findet sich jeweils wieder
; ein Binärbaum)
(define btree-of
  (lambda (t)
    (signature (mixed the-empty-tree
                      (node-of (btree-of t) t (btree-of t))))))

; Konstruiere ein Blatt mit Markierung x
(: make-leaf (%a -> (btree-of %a)))
(define make-leaf
  (lambda (x)
    (make-node empty-tree
               x
               empty-tree)))

; Beispielbaum: t1
(: t1 (btree-of real))
(define t1
  (make-node (make-node (make-leaf 1)
                        2
                        empty-tree)
             3
             (make-leaf 4)))

; Beispielbaum: t2
(: t2 (btree-of real))
(define t2
  (make-node (make-leaf 4)
             0
             (make-leaf 4)))

; Prozedur digit? gibt an, ob String ein einstellige Zahl ist
(: digit? (string -> boolean))
(check-expect (digit? "1") #t)
(check-expect (digit? "2") #t)
(check-expect (digit? "3") #t)
(check-expect (digit? "4") #t)
(check-expect (digit? "5") #t)
(check-expect (digit? "42") #f)
(check-expect (digit? "Baum") #f)
(define digit?
  (lambda (s)
    (cond
      ((string=? s "1") #t)
      ((string=? s "2") #t)
      ((string=? s "3") #t)
      ((string=? s "4") #t)
      ((string=? s "5") #t)
      ((string=? s "6") #t)
      ((string=? s "7") #t)
      ((string=? s "8") #t)
      ((string=? s "9") #t)
      ((string=? s "0") #t)
      (else #f))))

; Prädikat string-for-parse?
; soll überprüfen, ob übegebener String str geeignet, um Baum daraus zu konstruieren
;(: string-for-parse? (string -> boolean))
;
;(check-expect (string-for-parse? "_") #t)
;(check-expect (string-for-parse? "2") #f)
;(check-expect (string-for-parse? "(_2_)") #t)
;(check-expect (string-for-parse? "(_-2_)") #f)
;(check-expect (string-for-parse? "(_42_)") #f)
;(check-expect (string-for-parse? "(_4_)4(_4_)") #t)
;(check-expect (string-for-parse? "((_)_)") #f)
;(check-expect (string-for-parse? "(_2)9_2)2)") #f)
;
;(define string-for-parse?
;  (lambda (str)
;    (cond
;      ((string=? "_" str) #t)
;      (... (string->strings-list str) ...)
;      (else #f))))

; Funktion btree-parse
; akzeptiert einen String str und konstruiert einen Baum
;(: btree-parse (string -> (btree-of string)))
;
;(check-expect (btree-parse "_") empty-tree)
;(check-expect (btree-parse "(_1_)") (make-node (make-leaf 1)))
;(check-expect (btree-parse "(((_1_)2_)3(_4_))") t1)
;(check-expect (btree-parse "(_4_)0(_4_)") t2)
;
;(define btree-parse
;  (lambda (str)
;    (if (
    
              

