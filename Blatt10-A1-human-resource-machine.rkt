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

; --------------------------------------------------------------------------------------------------------------
; Draw and animate the office
; --------------------------------------------------------------------------------------------------------------

; Draw package
(: draw-package ((maybe-of package) -> image))
(define draw-package
  (lambda (p)
    (place-image (text (cond ((number? p) (number->string p))
                             ((string? p) p)
                             (else ""))
                       14 "black")
                 12 12
                 (overlay
                  (cond ((boolean? p) empty-image)
                        (else (rectangle 20 20 "solid" "lightgray")))
                  (rectangle 23 23 "solid" "brown")
                  (rectangle 24 24 "solid" "white")))))

; Draw list of packages
(: draw-pkgs (string (list-of (maybe-of package)) -> image))
(define draw-pkgs
  (lambda (lbl ps)
    (beside (place-image/align (text lbl 14 "black") (* 2.5 24) 12 "right" "center" (rectangle (* 2.5 24) 24 "solid" "white"))
            (empty-scene 3 0)
            (fold empty-image beside (map draw-package ps)))))

; Draw instruction based on instruction pointer and a given line number
(: draw-instruction ((maybe-of natural) -> (natural (mixed instruction string) -> image)))
(define draw-instruction
  (lambda (ip)
    (lambda (n instr)
      (let ((current? (and (number? ip) (= ip n))))
        (text (string-append
               (if current? "> " "   ")
               (if (< n 10) "0" "")
               (number->string n) ": "
               (cond ((string? instr)
                      (string-append "\"" instr "\""))
                     (else (description instr))))
              16
              "black")))))

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

; Draw the office
(: draw-office (office -> image))
(define draw-office
  (lambda (o)
    (above/align "left"
                 (text "Human Resource Machine Post Office" 30 "gray")
                 (empty-scene 0 6)
                 (draw-instructions (instruction-list o) (ip o))
                 (empty-scene 0 6)
                 (draw-pkgs "inbox <-" (inbox o))
                 (empty-scene 0 6)
                 (beside (draw-pkgs "worker" (list (worker o)))
                         (draw-pkgs "floor" (floor-slots o)))
                 (empty-scene 0 6)
                 (draw-pkgs "outbox ->" (outbox o))
                 (empty-scene 0 6)
                 (text (string-append "Time clock: " (number->string (time-clock o))) 14 "black")
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

; --------------------------------------------------------------------------------------------------------------
; Running the office
; --------------------------------------------------------------------------------------------------------------
; Hilsprozedur
; jump-in? schaut, ob eine Insturkiton "jump" enthalten ist in der Instrcution Liste
(: jump-in? (office -> boolean))
(define jump-in?
  (lambda (o)
    (cond 
      ((empty? (instruction-list o)) #f)   
      ((pair? (instruction-list o)) (let ((string-or-instr (if (string? (first (instruction-list o)))
                                                                 (first (instruction-list o))
                                                                 (description (first (instruction-list o))))))
                                    (cond
                                      ((string=? string-or-instr "jump") #t)
                                      ((string=? string-or-instr "jump-if-neg") #t)
                                      ((string=? string-or-instr "jump-if-zero") #t)
                                      (else (jump-in? (make-office (inbox o) (outbox o) (floor-slots o) (worker o) (rest (instruction-list o)) (ip o) (time-clock o))))))))))

; (c)
; Prozedur "perform-next" soll:
; - bei einem geg. Postamt o die nächste Arbeitsanweisung durchführen
; - wenn (ip o) den Wert #f hat, office bleibt unverändert
; - Stechuhr hochzählen
(: perform-next (office -> office))
(define perform-next
  (lambda (o)
    (if (boolean? (ip o))
        o
       (let ((>instr (list-ref (instruction-list o) (ip o)))) 
        (if (string? >instr)
            (make-office (inbox o)
                         (outbox o)
                         (floor-slots o)
                         (worker o)
                         (instruction-list o)
                         (+ (ip o) 1)
                         (time-clock o))
            (let ((run-instr ((action >instr) o))) 
              (make-office (inbox run-instr)
                           (outbox run-instr)
                           (floor-slots run-instr)
                           (worker run-instr)
                           (instruction-list o)
                           (ip run-instr)
                           (+ 1 (time-clock o)))))))))
                               
; (d)                                                                                                      
; Prozedur "perform-all" soll:
; - auf perform-next aufbauen
; - solange iterativ (jeweils nächste Instruktion ausführen) bis ip Wert #f

(define perform-all
  (lambda (o)
    (cond
      ((empty? (instruction-list o)) o)
      ((pair? (instruction-list o)) (if (jump-in? o)
                                        ((repeat (* (length (inbox o)) (length (instruction-list o))) perform-next) o)
                                        ((repeat (length (instruction-list o)) perform-next) o))))))

; --------------------------------------------------------------------------------------------------------------
; Office day1 Test
; --------------------------------------------------------------------------------------------------------------
; Office day 1
(define day01
  (make-office (list "E" 3) empty    ; inbox, outbox
               (replicate 16 #f) #f  ; floor, worker
               (list <-inbox         ; instructions:
                     ->outbox
                     <-inbox
                     ->outbox)            
               0 0))                 ; ip, time

(check-expect (outbox (perform-all day01)) (list 3 "E"))
(check-expect (inbox (perform-all day01)) empty)
(check-expect (inbox (perform-next day01)) (list 3))
(check-expect (inbox (perform-next (perform-next day01))) (list 3))
(check-expect (outbox (perform-next day01)) empty)
(check-expect (outbox (perform-next (perform-next day01))) (list "E"))
(check-expect (outbox (perform-next (perform-next (perform-next (perform-next day01))))) (list 3 "E"))
(check-expect (worker (perform-next day01)) "E")
(check-expect (worker (perform-next (perform-next day01))) #f)
(check-expect (ip (perform-next day01)) 1)
(check-expect (ip (perform-next (perform-next day01))) 2)
(check-expect (time-clock (perform-next day01)) 1)
(check-expect (time-clock (perform-next (perform-next day01))) 2)

;==================================================================================================================================
; Bonus              
;==================================================================================================================================

; (e)
; "verschönern" des office
;--------------------------------------------------------
; Boden des office
;--------------------------------------------------------
; definieren von "bodenkacheln"
(define tile-size 30)

(define tile1
  (square tile-size "solid" "burlywood"))

(define tile2
  (square tile-size "solid" "wheat"))

(define tile3
  (square tile-size "solid" "tan"))

(define tile-square1
  (above (beside tile1 tile2 tile3)
         (beside tile2 tile1 tile2)
         (beside tile3 tile2 tile1)))

(define tile-square2
  (above (beside tile2 tile3 tile1)
         (beside tile3 tile1 tile3)
         (beside tile1 tile3 tile2)))

(define tile-square3
  (above (beside tile3 tile1 tile3)
         (beside tile1 tile2 tile1)
         (beside tile2 tile1 tile3)))

(define office-floor
  (beside (empty-scene 40 0)
          (above (beside tile-square2 tile-square2 tile-square1 tile-square3)
                 (beside tile-square2 tile-square1 tile-square3 tile-square3)
                 (beside tile-square1 tile-square2 tile-square1 tile-square2))
          (empty-scene 40 0)
          (rectangle 250 380 "solid" "LightGoldenrodYellow")))

;--------------------------------------------------------
; Pakete
;--------------------------------------------------------
; zeichnet Pakete
(: draw-package2 ((maybe-of package) -> image))
(define draw-package2
  (lambda (p)
    (place-image (text (cond ((number? p) (number->string p))
                             ((string? p) p)
                             (else ""))
                       14 "black")
                 12 12
                 (overlay
                  (cond ((boolean? p) empty-image)
                        ((integer? p) (rectangle 20 20 "solid" "palegreen"))
                        (else (rectangle 20 20 "solid" "cornflowerblue")))
                  (cond ((boolean? p) (rectangle 23 23 "solid" "lavender" ))
                        ((integer? p) (rectangle 23 23 "solid" "limegreen"))
                        (else (rectangle 23 23 "solid" "royalblue")))
                  (rectangle 24 24 "solid" "white")))))

; Zeichnet eine Liste von Paketen für inbox,outbox
(: draw-pkgs2 ((list-of (maybe-of package)) -> image))
(define draw-pkgs2
  (lambda (ps)
      (fold empty-image above (map draw-package2 ps))))

; zeichnet liste von paketen für floor-slots
(: draw-pkgs3 ((list-of (maybe-of package)) -> image))
(define draw-pkgs3
  (lambda (ps)
      (above (fold empty-image beside (map draw-package2 (take 4 ps)))
             (fold empty-image beside (map draw-package2 (take 4 (drop 4 ps))))
             (fold empty-image beside (map draw-package2 (take 4 (drop 8 ps))))
             (fold empty-image beside (map draw-package2 (take 4 (drop 12 ps)))))))

;--------------------------------------------------------
; Inbox Outbox
;--------------------------------------------------------
; "Markierung" für Inbox, Outbox
; Winkel der Rotation von der Notiz: angle
; Wort/Notiz: str als string
(define draw-notice
  (lambda (angle str)
    (rotate angle (overlay (text str 25 "black")
                           (rectangle (* 15 (string-length str)) 30 "solid" "ivory")))))

; definieren eines "Fließbandes" für inbox und outbox
(define production-line1
  (let ((line1 (rectangle 35 25 "solid" "tan"))
        (line2 (rectangle 35 25 "solid" "blanchedalmond")))
  (overlay (above line1 line2 line1 line2 line1 line2)
           (rectangle 40 155 "solid" "black"))))

(define production-line2
  (let ((line1 (rectangle 35 25 "solid" "tan"))
        (line2 (rectangle 35 25 "solid" "blanchedalmond")))
  (overlay (above line2 line1 line2 line1 line2 line1)
           (rectangle 40 155 "solid" "black"))))

; zeichnet inbox des office mit seinen Paketen ps
(: draw-inbox ((list-of (maybe-of package)) -> image))
(define draw-inbox
  (lambda (ps)
    (beside (draw-notice 90 "INBOX->")
            (empty-scene 5 0)
            (overlay/xy (draw-pkgs2 ps)
                        -7 -4
                        (if (odd? (length ps))
                            production-line1
                            production-line2)))))

; zeichnet outbox des office mit seinen Paketen ps
(: draw-outbox ((list-of (maybe-of package)) -> image))
(define draw-outbox
  (lambda (ps)
    (beside (overlay/xy (draw-pkgs2 ps)
                        -7 -4
                        (if (odd? (length ps))
                            production-line2
                            production-line1))
            (empty-scene 5 0)
            (draw-notice -90 "OUTBOX->"))))

;--------------------------------------------------------
; floor-slots
;--------------------------------------------------------
; Zeichnet Slots auf dem Boden des office
(: draw-floor-slots ((list-of (maybe-of package)) -> image))
(define draw-floor-slots
  (lambda (ps)
    (above (text "floor-slots" 25 "black")
           (overlay (draw-pkgs3 ps)
                    (square 120 "solid" "peru")))))

;--------------------------------------------------------
; worker
;--------------------------------------------------------
; implementieren des "Körpers" vom Worker
(define hat
  (overlay/xy (square 15 "solid" "black")
              -5 12
              (rectangle 25 10 "solid" "black")))

(define eye-left
 (overlay/xy (circle 4 "solid" "black")
             -7 -7.7
            (overlay/xy (circle 8.5 "solid" "white")
                        -2 -2.2
                        (circle 11 "solid" "black"))))

(define eye-right
  (overlay/xy (ellipse 6 8 "solid" "black")
              -7 -7.7
              (overlay/xy (ellipse 14 15.5 "solid" "white")
                          -3.5 -2.2
                          (ellipse 19 21 "solid" "blacke"))))

(define worker-head
  (overlay/xy (rotate 180 (triangle 8 "solid" "darkred"))
              -32 -48
              (overlay/xy (rotate 25 hat)
                           0 18
                          (overlay/xy eye-right
                                      -35 -10
                                     (overlay/xy eye-left
                                                 -13 -10
                                                 (ellipse 50 40 "solid" "PapayaWhip"))))))

(define leg
  (overlay/xy (rectangle 10 20 "solid" "black")
              0 15
              (ellipse 20 10 "solid" "black")))
  
(define worker-body
  (overlay/xy worker-head
              12 47
              (overlay/xy (rhombus 15 30 "solid" "midnightblue")
                          -13 5
                          (overlay/xy (rectangle 20 20 "solid" "white")
                                      -6 0
                                      (underlay/xy leg
                                                   -2 -25
                                                  (underlay/xy leg
                                                               -18 -25
                                                              (rectangle 30 30 "solid" "black")))))))

(define arm
  (rotate 90 (isosceles-triangle 22 120 "solid" "black")))

(define hand
  (ellipse 20 15 "solid" "PapayaWhip"))

; Zeichnet worker mit einem beliebigen Paket p
(: draw-worker ((maybe-of package) -> image))
(define draw-worker
  (lambda (p)
    (overlay/xy (rotate 90 hand)
                -7 -62
                (overlay/xy (rotate 90 hand)
                            -34 -62
                            (overlay/xy (draw-package2 p)
                                         -15 -60
                                         (underlay/xy arm
                                                     -5 -45
                                                     (underlay/xy (rotate 180 arm)
                                                                 -40 -45
                                                                 worker-body)))))))


;--------------------------------------------------------
; instruction-list und instrcution-pointer
;--------------------------------------------------------
; zeichnet einzelne Instruktion bzw. String auf einen "Zettel"
(: draw-instr (instruction -> image))
(define draw-instr
  (lambda (instr)
    (if (string? instr)
        (overlay (text/font instr 16 "black" #f "swiss" "normal" "bold" #f)
                 (rectangle (* 10 (string-length instr)) 30 "solid" "GreenYellow"))
        (overlay (text/font (description instr) 16 "black" #f "swiss" "normal" "bold" #f)
                 (rectangle (* 10 (string-length (description instr))) 30 "solid"
                            (cond
                              ((string=? (description instr) "<-inbox") "gold")
                              ((string=? (description instr)  "->outbox") "orange red")
                              ((string=? (description instr) "jump") "green")
                              ((string=? (description instr) "jump-if-zero") "yellow")
                              ((string=? (description instr) "jump-if-neg") "pink")
                              ((string=? (description instr) "copy-from") "spring green")
                              ((string=? (description instr) "copy-to") "aqua")
                              ((string=? (description instr) "sub") "purple")
                              ((string=? (description instr) "add") "plum")
                              ((string=? (description instr) "bump+") "violet")
                              (else "moccasin")))))))

; Zeichnet Instruktion basierend auf Instruction Pointer (ip) und seinem Wert
(: draw-instruction2 ((maybe-of natural) -> (natural (mixed instruction string) -> image)))
(define draw-instruction2
  (lambda (ip)
    (lambda (n instr)
      (let ((current? (and (number? ip) (= ip n))))
        (beside (text/font (string-append
                           (if current? ">" " ")
                           (if (< n 10) "0" "")
                           (number->string n) ": ")
                            20 "black" #f "modern" "normal"
                           (if current? "bold" "normal") #f)
                (draw-instr instr))))))

; Zeichnet Liste von Instruktionen
(: draw-instructions2 ((list-of (mixed instruction string)) (maybe-of natural) -> image))
(define draw-instructions2
  (lambda (is ip)
    (above/align "left"
                 (text "Board of instructions:"  20 "black")
                 (text "(press any key to proceed,"  20 "black")
                 (text "ESC to finish work)"  20 "black")
                 (empty-scene 0 6)
                 (beside (empty-scene 12 0)
                         (fold empty-image
                               (lambda (instr res)
                                   (above/align "left" instr res))
                               (zipWith (draw-instruction2 ip)
                                        (range 0 (- (length is) 1))
                                        is)))            
                 (empty-scene 0 6))))

;--------------------------------------------------------
; time-clock
;--------------------------------------------------------
; implementieren der "Teile" einer Uhr
       ; Zeiger der Uhr
(define clock-hand (put-pinhole (/ 3 2)
                                (- 3 (* 1/16 28))
                                (rectangle 3 28 "solid" "DeepPink")))

       ; Markierungen der Uhr
(define m (put-pinhole 2
                        30
                        (rectangle 4 6 "solid" "orchid")))

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
                                          (circle 30 "solid" "lightpink")))

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

;--------------------------------------------------------
; office
;--------------------------------------------------------
; Prozedur zeichnet gegebenes office
(: draw-office2 (office -> image))
(define draw-office2
  (lambda (o)
    (above (text "Human Resource Machine Post Office" 30 "Peru")
           (overlay/xy (draw-inbox (inbox o))
                        -10 -160
                       (overlay/xy (draw-outbox (outbox o))
                                   -355 -160
                                   (overlay/xy (draw-worker (worker o))
                                                -100 -70
                                               (overlay/xy (draw-floor-slots (floor-slots o))
                                                           -160 -150
                                                           (overlay/xy (draw-instructions2 (instruction-list o) (ip o))
                                                                        -450 -10
                                                                        (overlay/xy (draw-time-clock (time-clock o))
                                                                                    -333 -65
                                                                                    office-floor)))))))))

; Prozedur animiert gegebenes office
(: start-office-day2 (office -> office))
(define start-office-day2
  (lambda (o)
    (big-bang o
              (to-draw draw-office2)
              (on-key (lambda (o key)
                        (cond ((key=? key "escape") (perform-all o))
                              (else (perform-next o))))))))