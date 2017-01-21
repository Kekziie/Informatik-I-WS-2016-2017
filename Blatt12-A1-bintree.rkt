;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt12-A1-bintree) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 1
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
  (make-node empty-tree
             1
             (make-node empty-tree
                        2.5
                        (make-leaf 3))))

; Beispielbaum: t2
(: t2 (btree-of real))
(define t2
  (make-node (make-leaf 4)
             -30
             (make-leaf 42)))

; Prozedur btree-min
; ermittelt minimalste Markierung eines Binärbaumes
;(: btree-min ((btree-of real) -> real))

;(check-expect (btree-min t1) 1)
;(check-expect (btree-min t2) -30)
;(check-expect (btree-min (make-node (make-leaf 3))) 3)

; Prozedur btree-max
; ermittelt maximalste Markierung eines Binärbaumes
;(: btree-max ((btree-of real) -> real))

;(check-expect (btree-max t1) 3)
;(check-expect (btree-max t2) 42)
;(check-expect (btree-max (make-node (make-leaf 3))) 3)