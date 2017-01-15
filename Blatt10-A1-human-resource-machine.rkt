;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt10-A1-human-resource-machine) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm")))))
; Aufgabe: Human Resource Machine

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

; A package contains eigther a number or a character
(define package (signature (mixed integer
                                  character)))

; A character is an upper case letter of the alphabet between "A" and "Z" 
(define character (signature (predicate
                              (lambda (c)
                                (and (string? c)
                                     (string<=? "A" c "Z"))))))
; A (maybe-of ⟨t⟩) is either an element of signature ⟨t⟩ or empty (#f)
(: maybe-of (signature -> signature))
(define maybe-of
  (lambda (t)
    (signature (mixed t (one-of #f)))))

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

;---------------------------------------------------------------------------------------------------------------------------------
; H.O.P. aus Vorlesung
;---------------------------------------------------------------------------------------------------------------------------------

; Hintereinanderschaltung von 2 Funktionen f(g(x))
(: compose ((%b -> %c) (%a -> %b) -> (%a -> %c)))
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

; Hintereinanderschaltung von Funktion f n-mal mit sich selbst
(: repeat (natural (%a -> %a) -> (%a -> %a)))
(check-expect ((repeat 10 (lambda (n) (+ n 1))) 0) 10)
(define repeat
  (lambda (n f)
    (cond
      ((= n 0) (lambda (x) x))
      ((> n 0) (compose (repeat (- n 1) f) f)))))

; =====================================================================================================================================

; Lösungen ab hier :D

;===========================================================================================================================================
; TAG 1
; Aufgabe "transportiert die ersten zwei Pakete der Inbox nacheinander auf die Outbox"
;===========================================================================================================================================

; ------------------------------------------------------------------------------------------------------
; first instructions
; ------------------------------------------------------------------------------------------------------

; (a) 
; Instruktion: <-inbox besthet aus:
; - Textdarstellung "<-inbox"
; - Funktion:
;      - aus geg. Office -> baut neues Office
;      - ip auf #f, wenn (inbox o) leer
;      - worker besetzt mit erstem Element von (inbox o) + inbox um ein Element verkürzt
(: <-inbox instruction)
(define <-inbox
  (make-instr "<-inbox"
              (lambda (o)
                 (if (empty? (inbox o))
                   (make-office (inbox o) (outbox o) (floor-slots o) (worker o) (instruction-list o) #f (time-clock o))
                   (make-office (rest (inbox o)) (outbox o) (floor-slots o) (first (inbox o)) (instruction-list o) (+ (ip o) 1) (time-clock o))))))

; (b)
; Instruktion: ->outbox besteht aus:
; - Textdarstellung: "->outbox"
; - Funktion:
;     - aus geg. Office -> baut neues Office
;     - worker legt Paket, das er trägt in outbox ab -> danach kein Paket
;     - wenn kein Paket davor -> Abbruch mit (violation "...")
(: ->outbox instruction)
(define ->outbox
  (make-instr "->outbox"
              (lambda (o)
              (if (empty? (worker o))
                  (violation "worker hat kein Paket")
                  (make-office (inbox o)
                               (make-pair (worker o) (outbox o))
                               (floor-slots o)
                                #f
                               (instruction-list o)
                               (+ (ip o) 1)
                               (time-clock o))))))