;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname human-resource-machine) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe
; Hilfsprozeduren
; a) <-inbox
; b) ->outbox
; c) perform-next
; d) perform-all
; e) BONUS Animation
; f) list-index
; g) jump
; h) Day02 
; i) jump-if-zero und jump-if-negative
; j) Day03
; k) list-update und copy-to
; l) copy-from
; m) Day 04
; n) ordinal,add, sub, bump+ und bump-
; o) Day05
; p) BONUS Day29

; --------------------------------------------------------------------------------------------------------------
; The office
; --------------------------------------------------------------------------------------------------------------

; A post office consists of
; - an inbox for incomming packages
; - an outbox for outgoing packages
; - a floor of fixed size to work on. Each slot contains either a package or nothing
; - a worker who can carry a single package (or nothing)
; - a list of instructions, telling the worker what to do step by step
; - an instruction pointer, telling the worker what to do next. It is either an absolute
;   position on the instructions board, starting from 0, or #f for "finish work!"
; - a time clock which records the total time of work (in number of performed instructions)
(define-record-procedures office
  make-office
  office?
  (inbox
   outbox
   floor-slots
   worker
   instruction-list
   ip
   time-clock))

(: make-office ((list-of package)
                (list-of package)
                (list-of (maybe-of package))
                (maybe-of package)
                (list-of (mixed instruction string))
                (maybe-of natural)
                natural
                -> office))
(: office? (any -> boolean))
(: inbox (office -> (list-of package)))
(: outbox (office -> (list-of package)))
(: floor-slots (office -> (list-of (maybe-of package))))
(: worker (office -> (maybe-of package)))
(: instruction-list (office -> (list-of (mixed instruction string))))
(: ip (office -> (maybe-of natural)))
(: time-clock (office -> natural))

; A package contains either a number or a character
(define package (signature (mixed integer character)))

; A character is an upper case letter of the alphabet between "A" and "Z" 
(define character (signature (predicate (lambda (c) (and (string? c) (string<=? "A" c "Z"))))))

; A (maybe-of ⟨t⟩) is either an element of signature ⟨t⟩ or empty (#f)
(: maybe-of (signature -> signature))
(define maybe-of
  (lambda (t)
    (signature (mixed t (one-of #f)))))


; --------------------------------------------------------------------------------------------------------------
; Some predefined list functions
; --------------------------------------------------------------------------------------------------------------

; replicate an element n times
(: replicate (natural %a -> (list-of %a)))
(define replicate
  (lambda (n x)
    (cond ((= n 0) empty)
          (else (make-pair x (replicate (- n 1) x))))))

; Zip two lists with a given function
(: zipWith ((%a %b -> %c) (list-of %a) (list-of %b) -> (list-of %c)))
(define zipWith
  (lambda (f xs ys)
    (cond ((empty? xs) empty)
          ((empty? ys) empty)
          (else        (make-pair (f (first xs) (first ys)) 
                                  (zipWith f (rest xs) (rest ys)))))))

; Return an integer list range
(: range (integer integer -> (list-of integer)))
(define range
  (lambda (from to)
    (cond ((> from to) empty)
          (else (make-pair from (range (+ from 1) to))))))

; (take w xs):
; - liefert die ersten w Elemente der Liste xs zurück
; - hat xs nur m < w Elemente, liefere diese m Elemente zurück
(: take (natural (list-of %a) -> (list-of %a)))
(check-expect (take 0 empty) empty)
(check-expect (take 0 (list 1)) empty)
(check-expect (take 2 (list 1)) (list 1))
(check-expect (take 2 (list 1 2)) (list 1 2))
(check-expect (take 2 (list 1 2 3)) (list 1 2))
(check-expect (take 3 (list 1 2 3 4 5 6 7 8 9)) (list 1 2 3))
(define take
  (lambda (w xs)
    (cond
      ((empty? xs) empty)
      ((= 0 w) empty)
      ((pair? xs) (if (<= (length xs) w)
                      xs
                      (make-pair (first xs)
                                 (take (- w 1) (rest xs))))))))  

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

; --------------
; Student task
; --------------

; Exercises (f) through (k): implement the higher-order procedures for list processing

; f) übergibt Position des ersten Elements, die auf das Prädikat p? zutrifft in einer Liste xs
(: list-index ((%a -> boolean) (list-of %a) -> natural))

(check-expect (list-index (lambda (x) (= 1 x)) (list 1 2 2)) 0)
(check-expect (list-index (lambda (x) (= 1 x)) (list 2 2 1)) 2)
(check-error(list-index (lambda (x) (= 1 x)) (list 2 2 2)) "no match")
(check-error(list-index (lambda (x) (= 1 x)) empty) "no match")

(define list-index
  (lambda (p? xs)
    (list-index-worker p? xs 0)))

(: list-index-worker ((%a -> boolean) (list-of %a) natural -> natural))
(define list-index-worker
  (lambda (p? xs acc)
    (cond
      ((empty? xs) (violation "no match"))
      ((p? (first xs)) acc)
      (else (list-index-worker p? (rest xs) (+ acc 1))))))

; k) tauscht das Element a an einer beliebigen Position x einer Liste xs aus
(: list-update ((list-of %a) natural %a -> (list-of %a)))

(check-expect (list-update (list "1" "2" "3") 0 "b") (list "b" "2" "3"))
(check-expect (list-update (list 1 2 3) 2 2) (list 1 2 2))
(check-error (list-update (list "1" "2" "3") 4 "b") "index too long")
(check-error (list-update empty 4 "b") "index too long")

(define list-update
  (lambda (xs x a)
    (list-update-worker xs x a empty)))

(: list-update-worker ((list-of %a) natural %a (list-of %a) -> (list-of %a)))
(define list-update-worker
  (lambda (xs x a acc)
    (cond
      ((or (empty? xs) (>= x (length xs))) (violation "index too long"))
      ((= x 0) (append (reverse acc) (list a) (rest xs)))
      (else (list-update-worker (rest xs) (- x 1) a (make-pair (first xs) acc))))))

; --------------------------------------------------------------------------------------------------------------
; Hilfsprozeduren
; --------------------------------------------------------------------------------------------------------------
; zählt instruction pointer und time-clock um 1 hoch
(: increase-ip-tc (office -> office))
(define increase-ip-tc
  (lambda (o)
    (cond ((boolean? (ip o)) o)
          (else 
           (make-office
            (inbox o)
            (outbox o)
            (floor-slots o)
            (worker o)
            (instruction-list o)
            (+ 1 (ip o))
            (+ 1 (time-clock o)))))))

; setzt ip eines office auf #f oder auf ein label
(: set-ip (office %a -> office))
(define set-ip
  (lambda (o a)
    (make-office
     (inbox o)
     (outbox o)
     (floor-slots o)
     (worker o)
     (instruction-list o)
     (if (boolean? a)
         #f
         (list-index
          (lambda (i) (and (string? i) (string=? i a)))
          (instruction-list o)))
     (time-clock o))))

; erstellt ein neues leeres office mit
; - 16 floor slots
; - worker fängt von vorne an
(: new-task ((list-of package) (list-of (mixed instruction string)) -> office))
(define new-task
  (lambda (ps is)
    (make-office ps empty (replicate 16 #f) 0 is 0 0)))

; zeigt Package vom Worker
; falls worker nichts hällt violation
(: worker-package (office -> package))
(define worker-package
  (lambda (o)
    (cond ((boolean? (worker o)) (violation "Arbeiter trägt kein Paket"))
          (else (worker o)))))

; An address is either a position of a target slot on the floor, or an indirect address
(define address (signature (mixed natural indirect)))

; An indirect address is a position of a slot on the floor which contains the
; -- possibly again indirect -- address of the target slot
(define-record-procedures indirect @ indirect? (un-@))
(: @ (address -> indirect))
(: indirect? (any -> boolean))
(: un-@ (indirect -> address))

; string representation of an address
(: address->string (address -> string))
(define address->string
  (lambda (addr)
    (cond ((indirect? addr) (string-append "(@ " (address->string (un-@ addr)) ")"))
          ((number? addr) (string-append (number->string addr))))))

; Return the position of a direct or indirect addressed slot on the floor
(: deref (office address -> natural))
(define deref
  (lambda (o addr)
    (cond ((indirect? addr)
           (let ((a (floor-slot-content o (deref o (un-@ addr)))))
             (cond ((number? a) a)
                   (else (violation (string-append "An Fußbodenabschnitt #"
                                                   (number->string (un-@ addr))
                                                   " befindet sich keine gültige Adresse"))))))
          (else addr))))

; gibt package eines Fußboden Slots zurück
; wenn sie leer ist -> #f
(: floor-slot-content (office address -> package))
(define floor-slot-content
  (lambda (o addr)
    (let* ((a (deref o addr))
           (s (if (>= a (length (floor-slots o)))
                  #f
                  (list-ref (floor-slots o) a))))
      (cond ((boolean? s)
             (violation (string-append "An Fußbodenabschnitt #"
                                       (number->string a)
                                       " befindet sich kein Paket")))
            (else s)))))

; Set the content of a floor slot of an office
; If the address is indirect, the slot address is taken from the content of the given slot
(: set-floor-slot (office address package -> office))
(define set-floor-slot
  (lambda (o addr p)
    (make-office
     (inbox o)
     (outbox o)
     (let ((a (deref o addr)))
       (if (>= a (length (floor-slots o)))
           (violation (string-append "Es existiert kein Fußbodenabschnitt #" (number->string a)))
           (list-update (floor-slots o) a p)))
     (worker o)
     (instruction-list o)
     (ip o)
     (time-clock o))))

; gibt dem worker ein package
(: set-worker (office (maybe-of package) -> office))
(define set-worker
  (lambda (o p)
    (make-office
     (inbox o)
     (outbox o)
     (floor-slots o)
     p
     (instruction-list o)
     (ip o)
     (time-clock o))))

; Apply a combinator function to the content of two packages.
; For characters use their position in the alphabet.
(: package-apply ((integer integer -> integer) package package -> integer))
(define package-apply
  (lambda (f p1 p2)
    (f (if (string? p1) (ordinal p1) p1)
       (if (string? p2) (ordinal p2) p2))))

; --------------------------------------------------------------------------------------------------------------
; Instructions
; --------------------------------------------------------------------------------------------------------------

; An instruction consists of
; - a text representation and
; - a function that modifies a given office, following the instruction
(define-record-procedures instruction
  make-instr
  instr?
  (description action))

(: make-instr (string (office -> office) -> instruction))
(: instr? (any -> boolean))
(: description (instruction -> string))
(: action (instruction -> (office -> office)))


; --------------
; Student task
; --------------

; Exercises (a), (b), (g), (i), (k), (l) and (n): implement the instructions

; a) falls inbox leer, ip auf #f
; ansonsten bekommt der worker das erste Element von der Inbox
(: <-inbox instruction)
(define <-inbox
  (make-instr "<-inbox"
              (lambda (o)
                (match (inbox o)
                  (empty (set-ip o #f))
                  ((make-pair y ys) (make-office
                                     ys
                                     (outbox o)
                                     (floor-slots o)
                                     y
                                     (instruction-list o)
                                     (ip o)
                                     (time-clock o)))))))
                                     
; b) falls worker #f violation
; ansonsten bekommt die Outbox das Element vom Worker
(: ->outbox instruction)
(define ->outbox
  (make-instr "->outbox"
              (lambda (o)
                (if (false? (worker o))
                    (violation "worker kein Paket")
                    (make-office
                     (inbox o)
                     (make-pair (worker o) (outbox o))
                     (floor-slots o)
                     #f
                     (instruction-list o)
                     (ip o)
                     (time-clock o))))))

; g) setzt ip auf den label
(: jump (string -> instruction))
(define jump
  (lambda (label)
    (make-instr (string-append "jump-if-negative \"" label  "\"")
                (lambda (o)
                  (set-ip o label)))))

; i)
; soll jump ausführen, wenn der worker 0 trägt
(: jump-if-zero (string -> instruction))
(define jump-if-zero
  (lambda (label)
    (make-instr
     (string-append "jump-if-zero \"" label "\"")
     (lambda (o)
       (cond ((and (number? (worker o)) (= (worker-package o) 0))
              ((action (jump label)) o))
             (else o))))))

; soll jump ausführen, wenn der worker negativen Wert trägt
(: jump-if-negative (string -> instruction))
(define jump-if-negative
  (lambda (label)
    (make-instr
     (string-append "jump-if-negative \"" label  "\"")
     (lambda (o)
       (cond ((and (number? (worker o)) (< (worker-package o) 0))
              ((action (jump label)) o))
             (else o))))))

; k)
; kopiert das Packet vom Worker auf floor-slot 
(: copy-to (address -> instruction))
(define copy-to
  (lambda (addr)
    (make-instr
     (string-append "copy-to " (address->string addr))
     (lambda (o)
       (set-floor-slot o addr (worker-package o))))))

; l)
; kopiert das Packet vom floor-slot in die Hand des Workers
(: copy-from (address -> instruction))
(define copy-from
  (lambda (addr)
    (make-instr
     (string-append "copy-from " (address->string addr))
     (lambda (o)
       (set-worker o (floor-slot-content o addr))))))

; addiert Floor-Slot mit worker Paketinhalt
(: add (address -> instruction))
(define add
  (lambda (addr)
    (make-instr
     (string-append "add " (address->string addr))
     (lambda (o)
       (set-worker o (package-apply + (worker-package o) (floor-slot-content o addr)))))))

; subtrahiert Floor-Slot vom worker Paketinhalt
(: sub (address -> instruction))
(define sub
  (lambda (addr)
    (make-instr
     (string-append "sub " (address->string addr))
     (lambda (o)
       (set-worker o (package-apply - (worker-package o) (floor-slot-content o addr)))))))
  
; erhöht floor-slot um 1 und worker hebt es dann auf
(: bump+ (address -> instruction))
(define bump+
  (lambda (addr)
    (make-instr
     (string-append "bump+ " (address->string addr))
     (lambda (o)
       (let ((p (package-apply + (floor-slot-content o addr) 1)))
         (set-worker (set-floor-slot o addr p) p))))))

; erniedrigt floor-slot um 1 und worker hebt es dann auf
(: bump- (address -> instruction))
(define bump-
  (lambda (addr)
    (make-instr
     (string-append "bump- " (address->string addr))
     (lambda (o)
       (let ((p (package-apply - (floor-slot-content o addr) 1)))
         (set-worker (set-floor-slot o addr p) p))))))


; n) gibt zu einem Buchstaben die Position im Alphabet aus
(: ordinal (character -> natural))
(define ordinal
  (lambda (c)
    (match c
      ("A" 1) ("B" 2) ("C" 3) ("D" 4) ("E" 5) ("F" 6) ("G" 7) ("H" 8)
      ("I" 9) ("J" 10) ("K" 11) ("L" 12) ("M" 13) ("N" 14) ("O" 15) ("P" 16)
      ("Q" 17) ("R" 18) ("S" 19) ("T" 20) ("U" 21) ("V" 22) ("W" 23) ("X" 24)
      ("Y" 25) ("Z" 26))))

; --------------------------------------------------------------------------------------------------------------
; Running the office
; --------------------------------------------------------------------------------------------------------------

; Perform the action of the next instruction of an office
(: perform-next (office -> office))
(define perform-next
  (lambda (o)
    (cond ((boolean? (ip o)) o) 
          ((>= (ip o) (length (instruction-list o))) (set-ip o #f)) 
          (else (increase-ip-tc
                 (let ((next (list-ref (instruction-list o) (ip o))))
                    (cond ((string? next) o) 
                          (else ((action next) o)))))))))

; Iteratively apply instructions to a given office
(: perform-all (office -> office))
(define perform-all
  (lambda (o)
    (cond ((boolean? (ip o)) o) 
          (else (perform-all (perform-next o))))))

; --------------------------------------------------------------------------------------------------------------
; Draw and animate the office
; --------------------------------------------------------------------------------------------------------------

; Bild vom Worker
(: worker-image image)
(define worker-image .)

; Draw worker
(: draw-worker ((maybe-of package) -> image))
(define draw-worker
  (lambda (p)
    (overlay/xy (draw-package p)
                -60 -15
                worker-image)))

; Draw package
(: draw-package ((maybe-of package) -> image))
(define draw-package
  (lambda (p)
    (place-image (text (cond ((number? p) (number->string p))
                             ((string? p) p)
                             (else ""))
                       35 "black")
                 21 23
                 (overlay
                  (cond ((boolean? p) empty-image)
                        (else (rectangle 40 40 "solid" "lightgray")))
                  (rectangle 43 43 "solid" "brown")
                  (rectangle 44 44 "solid" "white")))))

; Draw list of packages
(: draw-pkgs (string (list-of (maybe-of package)) -> image))
(define draw-pkgs
  (lambda (lbl ps)
    (beside (overlay (text lbl 20 "black") (rectangle 160 50 "solid" "darkkhaki"))
            (empty-scene 5 0)
            (fold empty-image beside (map draw-package ps)))))

; Draw instruction based on instruction pointer and a given line number
(: draw-instruction ((maybe-of natural) -> (natural (mixed instruction string) -> image)))
(define draw-instruction
  (lambda (ip)
    (lambda (n instr)
      (let ((current? (and (number? ip) (= ip n))))
        (text/font (string-append
                    (if current? ">" " ")
                    (if (< n 10) "0" "")
                    (number->string n) ": "
                    (cond ((string? instr)
                           (string-append "\"" instr "\""))
                          (else (description instr))))
                   16 "black" #f "modern" "normal"
                   (if current? "bold" "normal")
                   #f)))))

; Draw list of instructions
(: draw-instructions ((list-of (mixed instruction string)) (maybe-of natural) -> image))
(define draw-instructions
  (lambda (is ip)
    (above/align "left"
                 (text "Board of instructions: (press any key to proceed, ESC to finish work)"  14 "black")
                 (empty-scene 0 6)
                 (beside (empty-scene 12 0)
                         (fold empty-image
                               (lambda (instr res)
                                 (above/align "left" instr res))
                               (zipWith (draw-instruction ip)
                                        (range 0 (- (length is) 1))
                                        is)))            
                 (empty-scene 0 6))))

; floor-slots
; zeichnet liste von paketen für floor-slots
(: draw-floor-slots ((list-of (maybe-of package)) -> image))
(define draw-floor-slots
  (lambda (ps)
      (above (fold empty-image beside (map draw-package (take 4 ps)))
             (fold empty-image beside (map draw-package (take 4 (drop 4 ps))))
             (fold empty-image beside (map draw-package (take 4 (drop 8 ps))))
             (fold empty-image beside (map draw-package (take 4 (drop 12 ps)))))))

; time-clock
; implementieren der "Teile" einer Uhr
       ; Zeiger der Uhr
(define clock-hand (put-pinhole (/ 3 2)
                                (- 3 (* 1/16 28))
                                (rectangle 3 28 "solid" "Olive")))

       ; Markierungen der Uhr
(define m (put-pinhole 2
                        30
                        (rectangle 4 6 "solid" "DarkKhaki")))

       ; "Uhrscheibe"
(define clock-background (overlay/pinhole m
                                          (rotate 30 m)
                                          (rotate 60 m)
                                          (rotate 90 m)
                                          (rotate 120 m)
                                          (rotate 150 m)
                                          (rotate 180 m)
                                          (rotate 210 m)
                                          (rotate 240 m)
                                          (rotate 270 m)
                                          (rotate 300 m)
                                          (rotate 330 m)
                                          (circle 30 "solid" "snow")))

; Darstellung einer Uhr mit einem Zeiger in Abhängigkeit von Zeit t
(: clock (natural -> image))
(define clock
  (lambda (t)
    (clear-pinhole
      (overlay/pinhole
         (position-hand t)
         clock-background))))

; Position des Zeigers bei Zeit t
(: position-hand (natural -> image))
(define position-hand
  (lambda (t)
    (rotate (* t -30) clock-hand)))

; Prozedur zeichnet time-clock
(define draw-time-clock
  (lambda (n)
    (overlay (text/font (number->string n) 30 "black" #f "swiss" "normal" "bold" #f)
             (rotate 180 (clock n)))))

; Draw the office
(: draw-office (office -> image))
(define draw-office
  (lambda (o)
    (above/align "left"
                 (text "Human Resource Machine Post Office > BONUS!!! <" 30 "gray")
                 (empty-scene 0 6)
                 (beside/align "top"
                 (draw-instructions (instruction-list o) (ip o))
                 (draw-time-clock (time-clock o)))
                 (empty-scene 0 6)
                 (beside
                  (empty-scene 30 0)
                 (draw-pkgs "INBOX:" (inbox o)))
                 (empty-scene 0 6)
                 (beside
                 (draw-worker (worker o))
                 (empty-scene 10 0)
                 (draw-floor-slots (floor-slots o)))
                 (empty-scene 0 6)
                 (beside
                  (empty-scene 30 0)
                 (draw-pkgs "OUTBOX:" (outbox o)))
                 (empty-scene 0 10)
                 )))

; Animate the office
(: start-office-day (office -> office))
(define start-office-day
  (lambda (o)
    (big-bang o
      (to-draw draw-office)
      (on-key (lambda (o key)
                (cond ((key=? key "escape") (perform-all o))
                      (else (perform-next o))))))))


; --------------------------------------------------------------------------------------------------------------
; Programs and tests
; --------------------------------------------------------------------------------------------------------------

; Solution day 1
(define day01
  (make-office (list "E" 3) empty    ; inbox, outbox
               (replicate 16 #f) #f  ; floor, worker
               (list <-inbox         ; instructions:
                     ->outbox
                     <-inbox
                     ->outbox)            
               0 0))                 ; ip, time

(check-expect (outbox (perform-all day01)) (list 3 "E"))

; --------------
; Student task
; --------------

; Exercises (h), (j), (m) and (o): implement and test the worker's instructions

; h) Solution day 2
(define day02
  (new-task (list 1 2 "E" 3 8)
            (list "here"            
                  <-inbox             
                  ->outbox            
                  (jump "here"))))   

(check-expect (outbox (perform-all day02)) (list 8 3 "E" 2 1))

; j) Solution day 3
(define day03
  (new-task (list "E" -3 8 -1 0 "Z" -2 -10 -20 2)
            (list "begin"
                  <-inbox
                  (jump-if-negative "skip")
                  ->outbox
                  "skip"
                  (jump "begin"))))

(check-expect (outbox (perform-all day03)) (list 2 "Z" 0 8 "E"))

; m) Solution day 4
(define day04
  (new-task (list "E" -3 8 -1 0 "A")
            (list "begin"
                  <-inbox
                  (copy-to 0)
                  <-inbox
                  ->outbox
                  (copy-from 0)
                  ->outbox
                  (jump "begin")
                  )))

(check-expect (outbox (perform-all day04)) (list 0 "A" 8 -1 "E" -3))

; p) Solution day 29
(define day29
  (new-task (list "H" "A" "P" "P" "Y" "N" "E" "W" "Y" "E" "A" "R")
            (list "begin"
                  <-inbox
                  (copy-to 0)
                  (sub 0)
                  (copy-to 15)  
                  (copy-from 0)
                  ->outbox
                  "next-char"
                  <-inbox
                  (copy-to 13)  
                  (copy-from 15)
                  (copy-to 14)  
                  "next-i"
                  (copy-from 13)
                  (sub (@ 14))
                  (jump-if-zero "next-char")
                  (bump- 14)    
                  (jump-if-negative "append")
                  (jump "next-i")
                  "append"
                  (bump+ 15)
                  (copy-from 13)
                  (copy-to (@ 15))
                  ->outbox
                  (jump "next-char")
                  )))

(check-expect (outbox (perform-all day29)) (list "R" "W" "E" "N" "Y" "P" "A" "H"))
