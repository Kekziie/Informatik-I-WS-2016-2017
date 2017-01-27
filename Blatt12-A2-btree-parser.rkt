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

; Falte Baum t bzgl. z und c
(: btree-fold (%b (%b %a %b -> %b) (btree-of %a) -> %b))
(define btree-fold
  (lambda (z c t)
    (cond ((empty-tree? t) z)
          ((node? t)
           (c (btree-fold z c (node-left-branch t))
              (node-label t)
              (btree-fold z c (node-right-branch t)))))))

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

; (drop w xs)
; - verwirft die ersten w Elemente der Liste xs und gibt den Rest zurück
; - hat xs nur m < w Elemente, liefere die leere Liste zurück
(: drop (natural (list-of %a) -> (list-of %a)))
(check-expect (drop 0 empty) empty)
(check-expect (drop 0 (list 3)) (list 3))
(check-expect (drop 1 (list 1)) empty)
(check-expect (drop 2 (list 1 2 3)) (list 3))
(check-expect (drop 3 (list 1 2 3 4 5 6 7 8 9)) (list 4 5 6 7 8 9))
(define drop
  (lambda (w xs)
    (cond
      ((empty? xs) empty)
      ((= w 0) xs)
      ((pair? xs) (if (<= (length xs) w)
                       empty
                      (drop (- w 1) (rest xs)))))))

; Prozeduren verwerfen erstes bzw. letztes Element aus Liste xs
(define drop-first
  (lambda (xs)
    (drop 1 xs)))

(define drop-last
  (lambda (xs)
    (reverse (drop 1 (reverse xs)))))

; Prozedur "last" liefert letztes Element der Liste
(: last ((list-of %a) -> %a))
(check-expect (last empty) empty)
(check-expect (last (list 1)) 1)
(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list "a" "b")) "b")
(define last
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((empty? (rest xs)) (first xs))
      (else (last (rest xs))))))

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

; Prozedur löscht äußere Klammern in einer Liste
; wenn es keine äußeren Klammern gibt, dann wird die Liste zurück gegeben
(: delete-outer-bracket ((list-of string) -> (list-of string)))
(check-expect (delete-outer-bracket empty) empty)
(check-expect (delete-outer-bracket (list "1" "2" "3")) (list "1" "2" "3"))
(check-expect (delete-outer-bracket (list "(" "1" "2" ")")) (list "1" "2"))
(define delete-outer-bracket
  (lambda (xs)
    (cond
      ((empty? xs) empty)
      ((and (string=? (first xs) "(")
            (string=? (last xs) ")")) (drop-first (drop-last xs)))
      (else xs))))

; Prädikat leaf?
; ermittelt, ob string einem Blatt entspricht
(: leaf? (string -> boolean))
(define leaf?
  (lambda (str)
    (and (string=? (first (delete-outer-bracket (string->strings-list str))) "_")
         (digit? (first (rest (delete-outer-bracket (string->strings-list str)))))
         (string=? (last (delete-outer-bracket (string->strings-list str))) "_"))))

;=================================================================================================

; Funktion btree-parse
; akzeptiert einen String str und konstruiert einen Baum
(: btree-parse (string -> (btree-of string)))

(check-expect (btree-parse "_") empty-tree) ; (a)
(check-expect (btree-parse "(_1_)") (make-node empty-tree
                                               "1"
                                               empty-tree)) ; (b)
;(check-expect (btree-parse "(((_1_)2_)3(_4_))") t1) 
;(check-expect (btree-parse "(_4_)0(_4_)") t2)       
(check-error (btree-parse "((_))))_))5)") "String entspricht nicht der Regel")

(define btree-parse
  (lambda (str)
   (let ((del-bracket (delete-outer-bracket (string->strings-list str))))  
    (cond
      ((string=? str "_") empty-tree) ; (a)
      ((leaf? str) (make-leaf (first (rest (rest (string->strings-list str)))))) ; (b)
      ((string=? "_" (first del-bracket)) (make-node empty-tree
                                                    (first (drop-first del-bracket))
                                                    (btree-parse (drop 2 del-bracket))))
      ((string=? "_" (last del-bracket)) (make-node (btree-parse (reverse (drop 2 (reverse del-bracket)))
                                                                 (first (drop-first (reverse del-bracket)))
                                                                 empty-tree)))
      (else (violation "String entspricht nicht der Regel"))))))
       
    
    
              

