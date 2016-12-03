;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt06-A1-let) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 1

; Funktionen g,h und i beliebig

;(define f
;  (lambda (x)
;    (i (g x) (h (g x)))))

;Beispiel einer Funktion fBsp für eingesetze Funktionen g, h un i
(define fBsp
  (lambda (x)
    (- 1 (* 2 x) (+ 1 (* 2 x)))))

; (a) umformen von f, sodass (g x) nur einmal ausgewertet wird
;     benutzen von "let", nicht erlaubt

;(define f1
;  (lambda (x)
;     (i x (h x)
;        (g x))))

; Umformung vom Beispiel fBsp in f1Bsp
(define f1Bsp
  (lambda (x)
    (- 1 x (+ 1 x)
       (* 2 x))))

; (b) Begrüdung der Umformung und Bedeutung in 1a)

; die Umformung von 1a) ist deshalb zulässig,
; da die Prozedur f1 äquivalent zu f2 ist,
; d.h. das f1 ein "syntaktischer Zucker" von f2 ist

; (c) Umformung von f mit "let"

; (define f2
;   (lambda (x)
;     (let ((g1 (g x)))
;       (i g1 (h g1)))))

; Umformung vom Beispiel fBsp in f2Bsp
(define f2Bsp
  (lambda (x)
    (let ((g1 (* 2 x)))
      (- 1 g1 (+ 1 g1)))))

; anhand der Lösungen der Beispiele erkennt man,
; dass alle Prozeduren, dass gleiche Ergebnis zeigen 
(fBsp 2)
(f1Bsp 2)
(f2Bsp 2)

