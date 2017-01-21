;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt12-A3-searchtree) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 3
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

; (a)
; Prädikat search-tree?
; stellt fest, ob Binärbaum t, ein Suchbaum ist
; (ein Suchbaum ist, wenn alle Markierungen:
;                   - im linken Teilbaum kleiner sind als x
;                   - im rechten Teilbaum größer sind als x
;                   - nur einmal im Baum vorkommen)
;(: search-tree? ((btree-of real) -> boolean))

; (b)
; Prädikat search-tree-member?
; entscheidet, ob sich eine Markirung x in einem Suchbaum t befindet
;(: searchtree-member? (integer (btree-of integer) -> boolean))

; (c)
; Prozedur searchtree-insert
; fügt eine Markierung y in einem Suchbaum t ein
;(: searchtree-insert (integer (searchtree-of integer) -> (btree-of integer)))

; (d)
; Prozedur list->searchtree
; wandelt eine Liste von Elementen in einen Suchbaum
; (benutzen von fold)
;(: list->searchtree ((list-of integer) -> (btree-of integer)))

; (e)
; Prozedur searchtree-delete
; entfernt eine Markierung y aus einem Suchbaum t
(: searchtree-delete (integer (btree-of integer) -> (btree-of integer)))