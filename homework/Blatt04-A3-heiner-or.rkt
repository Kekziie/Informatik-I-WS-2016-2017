;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt04-A3-heiner-or) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 3

(define heiner-or
  (lambda (test-1 test-2)
    (if test-1
        #t
        test-2)))

(heiner-or (= 10 10) (> 2 5))

(heiner-or (> 23 42) (< 5 2))

; Eva-Lu hat Recht
; am unteren Programm sieht man, dass or, wenn der erste Ausdruck #t wird, der 2. Ausdruck nicht beachtet wird
; bei heiner-or hingegen, werden beide Ausdrücke angeschaut, d.h. es ensteht im meinem Beispiel eine Dauerschleife

; d.h. heiner-or und or ist nicht das Gleiche

; Dauerschleife
(define loop
  (lambda (n)
    (loop (+ n 1))))

; or evaluiert zu #t
(or (> 10 9) (loop 1))

; heiner-or terminiert nicht
(heiner-or (> 10 9) (loop 1))

           