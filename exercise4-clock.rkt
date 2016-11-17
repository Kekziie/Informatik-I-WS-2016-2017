;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname exercise4-clock) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; animieren sie ein Uhr mithilfe der Funktion von "pinhole"

; definieren der Größe, des Modus und der Farbe

(define second-hand-width 6)
(define second-hand-length 90)
(define minute-hand-width 10)
(define minute-hand-length 80)
(define hour-hand-width 16)
(define hour-hand-length 70)
(define hour-mark-width 10)
(define hour-mark-length 20)
(define clock-radius 100)

(define mode "solid")

(define second-hand-color "violet red")
(define minute-hand-color "orchid")
(define hour-hand-color "lightpink")
(define hour-mark-color "midnight blue")
(define clock-color "Lavender")

; "malen" der Uhr
(define second-hand (put-pinhole 3 85 (rectangle second-hand-width second-hand-length mode second-hand-color))) ; Sekundenzeiger
(define minute-hand (put-pinhole 5 75 (rectangle minute-hand-width minute-hand-length mode minute-hand-color))) ; Minutenzeiger
(define hour-hand (put-pinhole 8 65 (rectangle hour-hand-width hour-hand-length mode hour-hand-color))) ; Stundenzeiger
(define hm (put-pinhole 5 clock-radius (rectangle hour-mark-width hour-mark-length mode hour-mark-color))) ;  "Stundenmarkierungen" (hour-mark hm)
; Hintergrund der Uhr mit "Stundenmarkierungen"
(define clock-background (overlay/pinhole hm
                                          (rotate 30 hm)
                                          (rotate 60 hm)
                                          (rotate 90 hm)
                                          (rotate 120 hm)
                                          (rotate 150 hm)
                                          (rotate 180 hm)
                                          (rotate 210 hm)
                                          (rotate 240 hm)
                                          (rotate 270 hm)
                                          (rotate 300 hm)
                                          (rotate 330 hm)
                                          (circle clock-radius mode clock-color)))

; Darstellung der Uhr in Abhängigkeit von der Zeit t

(: clock (natural -> image))

(define clock
  (lambda (t)
    (clear-pinhole
     (overlay/pinhole
      (seconds t)
      (minutes t)
      (hours t)
      clock-background))))

; rechnet die "echte" Zeit aus Stunden, Minuten und Sekunden nach Ticks um
; (Funktion "animate" führt 28 Ticks pro Sekunde aus)

(: time->ticks (natural natural natural -> natural))

(check-expect (time->ticks 0 0 0) 0)
(check-expect (time->ticks 0 0 1) 28)
(check-expect (time->ticks 1 1 1) 102508)
(check-expect (time->ticks 2 1 0) 203280)

(define time->ticks
  (lambda (h m s)
    (* (+ (* h 3600)
          (* m 60)
          s)
       28)))

; rechnet den Wert t in "echte" (a) Sekunden (sec)
;                               (b) Minuten (min)
;                               (c) Stunden (hour)

; (a)
(: t->sec (natural -> natural))

(check-expect (t->sec 5) 0)
(check-expect (t->sec 28) 1)
(check-expect (t->sec 56) 2)
(check-expect (t->sec 0) 0)
(check-expect (t->sec 500) 17)

(define t->sec
  (lambda (t)
    (quotient t 28)))

; (b)
(: t->min (natural -> natural))

(check-expect (t->min 0) 0)
(check-expect (t->min 1000) 0)
(check-expect (t->min 2800) 1)
(check-expect (t->min 10000) 5)
(check-expect (t->min 900000) 535)

(define t->min
  (lambda (t)
    (quotient (t->sec t) 60)))

; (c)
(: t->hour (natural -> natural))

(check-expect (t->hour 5) 0)
(check-expect (t->hour (time->ticks 5 17 42)) 5)
(check-expect (t->hour 3000) 0)

(define t->hour
  (lambda (t)
    (quotient (t->sec t) 3600)))

; Position des Sekundenzeigers

(: seconds (natural -> image))

(check-expect (seconds (time->ticks 0 0 42)) (rotate -252 second-hand))
(check-expect (seconds (time->ticks 2 5 0)) (rotate 0 second-hand))

(define seconds
  (lambda (t)
    (rotate (* (t->sec t) -6) second-hand)))

; Position des Minutenzeigers

(: minutes (natural -> image))

(check-expect (minutes (time->ticks 0 0 42)) (rotate 0 minute-hand))
(check-expect (minutes (time->ticks 0 17 42)) (rotate -102 minute-hand))

(define minutes
  (lambda(t)
    (rotate (* (t->min t) -6) minute-hand)))

; Position des Stundenzeigers

(: hours (natural -> image))

(check-expect (hours (time->ticks 0 0 42)) (rotate 0 hour-hand))
(check-expect (hours (time->ticks 6 0 42)) (rotate -180 hour-hand))

(define hours
  (lambda(t)
    (rotate (* (t->hour t) -30) hour-hand)))

(animate clock)