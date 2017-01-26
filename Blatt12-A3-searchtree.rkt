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
; Prozedur contains? schaut, ob Element x in Liste xs mit einer Vergleichsprozedur f
(: contains? (%a (%a %a -> boolean) (list-of %a) -> boolean))
(check-expect (contains? "a" string=? (list "a" "b")) #t)
(check-expect (contains? 2 = (list 1 2 3)) #t)
(check-expect (contains? 3 = empty) #f)
(check-expect (contains? 42 = (list 0 -1 1)) #f)
(define contains?
  (lambda (z c xs)
    (fold #f (lambda (a b)
               (or (c z a) b)) xs)))

; Prozedur distinct? schaut, ob es Duplikate gibt
(: distinct? ((%a %a -> boolean) (list-of %a) -> boolean))
(check-expect (distinct? = (list 1 2 3)) #f)
(check-expect (distinct? = (list 1 1 1)) #t)
(check-expect (distinct? string=? (list "a" "b" "a")) #t)
(check-expect (distinct? = empty) #f)
(check-expect (distinct? = (list 1 2 3 3)) #t)
(define distinct?
  (lambda (c xs)
    (cond
      ((empty? xs) #f)
      ((contains? (first xs) c (rest xs)) #t)
      (else (distinct? c (rest xs))))))

; Prozedur list-check-min
; - akzeptiert ein Zahl x und eine Liste xs
; - kontrolliert, ob alle Elemente links von Zahl x kleiner als x sind
; - bei leerer Liste wird #t ausgegeben
(: list-check-min (real (list-of real) -> boolean))
(check-expect (list-check-min 4 (list 1 2 3 4 5 6 7)) #t)
(check-expect (list-check-min 3 (list 9 3 4)) #f)
(check-expect (list-check-min 3 empty) #t)
(check-expect (list-check-min 0 (list -1 3 0 5 6)) #f)
(define list-check-min
  (lambda (x xs)
    (cond
      ((empty? xs) #t)
      ((> (first xs) x) #f)
      ((= x (first xs)) #t)
      (else (list-check-min x (rest xs))))))

; Prozedur list-check-max
; - akzeptiert ein Zahl x und eine Liste xs
; - kontrolliert, ob alle Elemente rechts von Zahl x größer als x sind
; - bei leerer Liste wird #t ausgegeben
(: list-check-max (real (list-of real) -> boolean))
(check-expect (list-check-max 4 (list 1 2 3 4 5 6 7)) #t)
(check-expect (list-check-max 3 (list 9 3 4)) #t)
(check-expect (list-check-max 3 empty) #t)
(check-expect (list-check-max 0 (list -1 3 0 5 6)) #t)
(define list-check-max
  (lambda (x xs)
    (cond
      ((empty? xs) #t)
      ((< (first (reverse xs)) x) #f)
      ((= x (first (reverse xs))) #t)
      (else (list-check-max x (reverse (rest (reverse xs))))))))

; Prozedur btree-min
; ermittelt minimalste Markierung eines Binärbaumes
(: btree-min ((btree-of real) -> real))
(check-within (btree-min t1) -10 0.01)
(check-within (btree-min t2) 0 0.01)
(check-within (btree-min t3) -20 0.01)
(define btree-min
  (lambda (btree)
    (btree-fold +inf.0 min btree)))

; Prädikat leaf?
; ermittelt, ob geg. Knoten K ein Blatt ist
(: leaf? ((btree-of %a) -> boolean))
(check-expect (leaf? (node-left-branch t1)) #t)
(check-expect (leaf? (node-right-branch t1)) #t)
(check-expect (leaf? t1) #f)
(define leaf?
  (lambda (k)
    (= 1 (length (inorder k)))))
 
; (a)
; Prädikat search-tree?
; stellt fest, ob Binärbaum t, ein Suchbaum ist
; (ein Suchbaum ist, wenn alle Markierungen:
;                   - im linken Teilbaum kleiner sind als x
;                   - im rechten Teilbaum größer sind als x
;                   - nur einmal im Baum vorkommen)
(: search-tree? ((btree-of real) -> boolean))

(check-expect (search-tree? t1) #t)
(check-expect (search-tree? t2) #f)
(check-expect (search-tree? t3) #t)
(check-expect (search-tree? t4) #f)
(check-expect (search-tree? t5) #t)
(check-expect (search-tree? empty-tree) #t)

(define search-tree?
  (lambda (btree)
   (let ((list-btree (inorder btree))) 
    (cond
      ((empty-tree? btree) #t)
      ((distinct? = list-btree) #f)
      ((not (list-check-min (node-label btree) list-btree)) #f)
      ((not (list-check-max (node-label btree) list-btree)) #f)
      (else #t)))))
    
; (b)
; Prädikat search-tree-member?
; entscheidet, ob sich eine Markirung x in einem Suchbaum t befindet
(: searchtree-member? (integer (btree-of integer) -> boolean))

(check-expect (searchtree-member? -10 t1) #t)
(check-expect (searchtree-member? 10 t1) #t)
(check-expect (searchtree-member? 42 t1) #f)
(check-expect (searchtree-member? 100 t3) #f)
(check-expect (searchtree-member? 3 t3) #t)
(check-expect (searchtree-member? -80 t5) #t)
(check-error (searchtree-member? 0 t2) "Baum ist kein Suchbaum")

(define searchtree-member?
  (lambda (x t)
    (if (search-tree? t)
        (contains? x = (inorder t))
        (violation "Baum ist kein Suchbaum"))))

; (c)
; Signatur searchtree-of
(define searchtree-of
  (lambda (t)
    (signature (combined (btree-of t)
                         (predicate search-tree?)))))

; Prozedur searchtree-insert
; fügt eine Markierung x in einem Suchbaum t ein
(: searchtree-insert (integer (searchtree-of integer) -> (searchtree-of integer)))

(check-expect (searchtree-insert 0 t1) t1)
(check-expect (searchtree-insert -10 t1) t1)
(check-expect (searchtree-insert 10 empty-tree) (make-leaf 10))
(check-expect (searchtree-insert 10 t1) t1)
(check-expect (searchtree-insert 100 t1) (make-node (make-leaf -10)
                                                    0
                                                    (make-node empty-tree
                                                               10
                                                               (make-leaf 100))))
(check-expect (searchtree-insert -100 t1) (make-node (make-node (make-leaf -100)
                                                                -10
                                                                empty-tree)
                                                     0
                                                     (make-leaf 10)))

(define searchtree-insert
  (lambda (x t)
    (cond
      ((empty-tree? t) (make-leaf x))
      ((= (node-label t) x) t)
      ((< x (node-label t)) (make-node (searchtree-insert x (node-left-branch t))
                                       (node-label t)
                                       (node-right-branch t)))
      ((> x (node-label t)) (make-node (node-left-branch t)
                                       (node-label t)
                                       (searchtree-insert x (node-right-branch t)))))))

; (d)
; Prozedur list->searchtree
; wandelt eine Liste von Elementen in einen Suchbaum
; (benutzen von fold)
(: list->searchtree ((list-of integer) -> (searchtree-of integer)))

(check-expect (list->searchtree (list 1 2 3)) (make-node empty-tree
                                                         1
                                                         (make-node empty-tree
                                                                    2
                                                                    (make-leaf 3))))
(check-expect (list->searchtree (list 1 0 -1)) (make-node (make-node (make-leaf -1)
                                                                     0
                                                                     empty-tree)
                                                          1
                                                          empty-tree))
(check-expect (list->searchtree (list 15 0 -3 42)) (make-node (make-node (make-leaf -3)
                                                                         0
                                                                         empty-tree)
                                                              15
                                                              (make-leaf 42)))

(define list->searchtree
  (lambda (xs)
    (fold empty-tree searchtree-insert (reverse xs))))

; (e)
; Prozedur searchtree-delete
; entfernt eine Markierung x aus einem Suchbaum t
(: searchtree-delete (integer (searchtree-of integer) -> (searchtree-of integer)))

(check-within (searchtree-delete 0 empty-tree) empty-tree 0.01)
(check-within (searchtree-delete -10 t1) (make-node empty-tree
                                                    0
                                                    (make-leaf 10)) 0.01)
(check-within (searchtree-delete 10 t1) (make-node (make-leaf -10)
                                                   0
                                                   empty-tree) 0.01)
(check-within (searchtree-delete 2 t5) (make-node (make-node (make-leaf -2)
                                                              -80
                                                              empty-tree)
                                                  1
                                                 (make-node (make-node (make-leaf 100)
                                                                        50
                                                                       (make-leaf 30))
                                                             33
                                                             empty-tree)) 0.01)
(check-within (searchtree-delete 0 t1) (make-node (make-leaf -10)
                                                  10
                                                  empty-tree) 0.01)
(check-within (searchtree-delete 1 t5) (make-node (make-node (make-leaf -2)
                                                             -80
                                                             empty-tree)
                                                  33
                                                  (make-node (make-node (make-leaf 100)
                                                                        50
                                                                        (make-leaf 30))
                                                             2
                                                             empty-tree)) 0.01)

(define searchtree-delete
  (lambda (x t)
     (cond
        ((empty-tree? t) empty-tree)
        ((> x (node-label t)) (make-node (node-left-branch t)
                                         (node-label t)
                                         (searchtree-delete x (node-right-branch t))))
        ((> x (node-label t)) (make-node (searchtree-delete x (node-left-branch t))
                                         (node-label t)
                                         (node-right-branch t)))
        ((and (= x (node-label t)) (empty-tree? (node-left-branch t))) (node-right-branch t))
        ((and (= x (node-label t)) (empty-tree? (node-right-branch t))) (node-left-branch t))
        (else (make-node (node-left-branch t)
                         (btree-min (node-right-branch t))
                         (searchtree-delete (btree-min (node-right-branch t))
                                            (node-right-branch t)))))))
                         

             
                        