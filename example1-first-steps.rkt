;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname example1-first-steps) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Beispiele 1

; i) Bindung von Namen (identifier) an Werten
;    (define {id} {expression})

(define pi 3.1415)
(define absoluter-nullpunkt -273.15)
(define minutes-in-a-day (* 24 60))
(define Uni "Universität")
(define Quadrat1 (square 25 "solid" "black"))

; ii) schreiben von "kleinen" Prozeduren nach Konstruktionsanleitung
; Konstruktionsanleitung: 1) Kurzbeschreibung
;                         2) Signatur
;                         3) Testfälle
;                         4) Prozedur

; Um wieviele Menschen wächst die Weltbevölkerung in Tagen (days)?
(: population-growth-in-days (natural -> natural))

(check-expect (population-growth-in-days 0)  0)
(check-expect (population-growth-in-days 1)  223200)
(check-expect (population-growth-in-days 5)  1116000)
(check-expect (population-growth-in-days 10) 2232000)

(define population-growth-in-days
  (lambda (days)
    (* days (* 155 minutes-in-a-day))))

; Flächeninhalt eines Kreises mit Radius r
(: circle-area (real -> real))

(check-within (circle-area 24.62) 1904.202 0.001)  
(check-within (circle-area 1)     pi       0.001)
(check-within (circle-area 5)     78.5375  0.001)
(check-within (circle-area 10)    314.15   0.001)
(check-within (circle-area 2.5)   19.63437 0.001)

(define circle-area
  (lambda (r)
    (* pi (* r r))))





