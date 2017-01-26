;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt12-A3-searchtree) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 3

; Definitionen aus Skript
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

; Liste der Markierungen in t in Inorder-Reihenfolge
(: inorder ((btree-of %a) -> (list-of %a)))
(define inorder
  (lambda (t)
    (btree-fold empty
                (lambda (xs1 x xs2)
                  (append 
                   xs1      
                   (list x) 
                   xs2      
                   ))
                t)))

; Falte Baum t bzgl. z und c
(: btree-fold (%b (%b %a %b -> %b) (btree-of %a) -> %b))
(define btree-fold
  (lambda (z c t)
    (cond ((empty-tree? t) 
           z)
          ((node? t)
           (c (btree-fold z c (node-left-branch t))
              (node-label t)
              (btree-fold z c (node-right-branch t)))))))

; -------------------------------------------------------------------

; Beispielbaum: t1
(: t1 (btree-of real))
(define t1
  (make-node (make-leaf -10)
             0
             (make-leaf 10)))

; Beispielbaum: t2
(: t2 (btree-of real))
(define t2
  (make-node (make-leaf 20)
             0
             (make-leaf 10)))

; Beispielbaum: t3
(: t3 (btree-of real))
(define t3
  (make-node (make-node (make-leaf -20)
                        -10
                        (make-leaf -1))
             0
             (make-node (make-leaf 1)
                        2
                        (make-leaf 3))))

; Beispielbaum: t4
(: t4 (btree-of real))
(define t4
  (make-node (make-leaf -1)
             -1
             (make-leaf 10)))

; Beispielbaum: t5
(: t5 (btree-of real))
(define t5
  (make-node (make-node (make-leaf -2)
                        -80
                        empty-tree)
             1
             (make-node (make-node (make-leaf 100)
                                   50
                                   (make-leaf 30))
                        33
                        (make-leaf 2))))

; -------------------------------------------------------------------
; Hilfsporzeduren
; Prozedur root
; - akzpetiert einen Baum
; - gibt die Wurzel zurück
(: root ((btree-of %a) -> %a))

(check-expect (root t1) 0)
(check-expect (root t2) 0)
(check-expect (root t4) -1)
(check-expect (root t5) 1)

(define root
  (lambda (btree)
    (node-label btree)))

; (a)
; Prädikat search-tree?
; stellt fest, ob Binärbaum t, ein Suchbaum ist
; (ein Suchbaum ist, wenn alle Markierungen:
;                   - im linken Teilbaum kleiner sind als x
;                   - im rechten Teilbaum größer sind als x
;                   - nur einmal im Baum vorkommen)
;(: search-tree? ((btree-of real) -> boolean))
;
;(check-expect (search-tree? t1) #t)
;(check-expect (search-tree? t2) #f)
;(check-expect (search-tree? t3) #t)
;(check-expect (search-tree? t4) #f)
;(check-expect (search-tree? t5) #t)
;(check-expect (search-tree? empty-tree) #t)
;
;(define search-tree?
;  (lambda (btree)
;    (inorder btree)))
    
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
;(: searchtree-delete (integer (btree-of integer) -> (btree-of integer)))