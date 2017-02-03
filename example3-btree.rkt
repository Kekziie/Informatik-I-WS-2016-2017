;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname example3-btree) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Beispiele 3 - Binärbäume

;----------------------------------------------------------------------------
; Definition: Binärbaum
;----------------------------------------------------------------------------

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

;----------------------------------------------------------------------------
; H.O.P. mit Binärbäumen
;----------------------------------------------------------------------------

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

;----------------------------------------------------------------------------
; Größe und Tiefe eines Binärbaumes
;----------------------------------------------------------------------------

; Tiefe eines Binärbaumes t
; (Länge des längsten Weges in Baum t von der Wurzel bis zu einem Blatt)
(: btree-depth ((btree-of %a) -> natural))
(define btree-depth
  (lambda (t)
    (btree-fold 0
                (lambda (d1 x d2) (+ 1 (max d1 d2)))
                t)))

; Größe eines Binärbaumes t
; (Anzahl der Knoten in t)
(: btree-size ((btree-of %a) -> natural))
(define btree-size
  (lambda (t)
    (cond
      ((empty-tree? t) 0)
      ((node? t) (+ (btree-size (node-left-branch t))
                    1
                    (btree-size (node-right-branch t)))))))

;----------------------------------------------------------------------------
; "Darstellung" eines Binärbaumes
;----------------------------------------------------------------------------

; Hilfsfunktion: konkateniere String s genau n mal
(: duplicate (natural string -> string))
(define duplicate
  (lambda (n s)
    (cond ((= n 0) "")
          ((> n 0) (string-append s (duplicate (- n 1) s))))))

; Hilfsprozedur: curry wendet Funktion auf ihr erstes Argument an und
;                liefert eine Funktion der restlichen Argumente
(: curry ((%a %b -> %c) -> (%a -> (%b -> %c))))
(define curry
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (f x y)))))

; Erzeuge Liste von Zeilen-Strings der Textrepräsentation des Baums t
(: pp ((btree-of (mixed number string)) -> (list-of string)))
(define pp
  (lambda (t)
    (cond 
      ((empty-tree? t) (list "▢\n"))
      ((node? t)
       (letrec ((lbl (node-label t))
                (x   (if (string? lbl) lbl (number->string lbl)))
                (wx  (string-length x))
                (ppl (pp (node-left-branch t)))
                (ppr (pp (node-right-branch t))))
         (append (list (string-append x "--"                         (first ppr)))
                 (map ((curry string-append) 
                       (string-append "|" (duplicate (+ 1 wx) " "))) (rest ppr))
                 (list                "│\n")
                 (list (string-append "╰" (duplicate (+ 1 wx) "-")   (first ppl)))
                 (map ((curry string-append) 
                       (string-append " " (duplicate (+ 1 wx) " "))) (rest ppl))))))))

;                                          Prozedur hat keinen Rückgabewert
; Drucke Textrepräsentation des Baums t         ↓
(: print ((btree-of (mixed number string)) -> %void))
(define print                               
  (lambda (t)
    (write-string (strings-list->string (pp t))))) 

; "Missbrauche" check-property, um einige Testbäume auszudrucken
; (check-property
;  (for-all ((t (btree-of natural)))
;     (==> (< (btree-size t) 10)
;          (expect (print t) (write-string "\n")))))

;----------------------------------------------------------------------------
; Baumdurchläufe
;----------------------------------------------------------------------------

; 1) Tiefendurchlauf

; Liste der Markierungen in t in Inorder-Reihenfolge
(: inorder ((btree-of %a) -> (list-of %a)))
(define inorder
  (lambda (t)
    (btree-fold empty
                (lambda (xs1 x xs2)
                  (append xs1      
                         (list x) 
                         xs2))
                t)))

; Liste der Markierungen in t in Preorder-Reihenfolge
(: preorder ((btree-of %a) -> (list-of %a)))
(define preorder
  (lambda (t)
    (btree-fold empty
                (lambda (xs1 x xs2)
                  (append (list x) 
                          xs1      
                          xs2))
                t)))

; Liste der Markierungen in t in Postorder-Reihenfolge
(: postorder ((btree-of %a) -> (list-of %a)))
(define postorder
  (lambda (t)
    (btree-fold empty
                (lambda (xs1 x xs2)
                  (append xs1      
                          xs2      
                         (list x)))
                t)))

; 2) Breitendurchlauf

; Hilfsprozedur: filter extrahiert Elemente einer Liste xs, die das Prädikat p? erfüllen
(: filter ((%a -> boolean) (list-of %a) -> (list-of %a)))
(define filter
  (lambda (p? xs)
    (cond ((empty? xs) empty)
          ((p? (first xs)) (make-pair (first xs) (filter p? (rest xs))))
          (else (filter p? (rest xs))))))

; Hilfsprozedur: flatten macht aus einer Liste von Listen eine Liste
(: flatten ((list-of (list-of %a)) -> (list-of %a)))
(define flatten
  (lambda (xss)
    (fold empty append xss)))

; Breitendurchlauf für die Liste der Bäume ts
(: traverse ((list-of (btree-of %a)) -> (list-of %a)))
(define traverse
  (lambda (ts)
    (cond ((empty? ts) empty)
          ((pair? ts)  (append (roots ts) 
                               (traverse (subtrees ts)))))))

; Liste der Wurzelmarkierungen der nicht-leeren Bäume in ts
(: roots ((list-of (btree-of %a)) -> (list-of %a)))
(define roots
  (lambda (ts)
    (map node-label 
         (filter node? ts))))

  
; Liste der Teilbäume der nicht-leeren Bäume in ts
(: subtrees ((list-of (btree-of %a)) -> (list-of (btree-of %a))))
(define subtrees
  (lambda (ts)
    (flatten 
     (map (lambda (t) (list (node-left-branch t)
                            (node-right-branch t)))
          (filter node? ts)))))

; Breitendurchlauf für den Baum t
(: levelorder ((btree-of %a) -> (list-of %a)))
(define levelorder
  (lambda (t)
    (traverse (list t))))