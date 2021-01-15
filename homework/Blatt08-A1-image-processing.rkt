;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt08-A1-image-processing) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 8
; arbeiten mit Bildern:
; - vertikal spiegeln
; - horizontal spiegeln
; - 90° gegen Uhrzeigersinn rotieren

; definieren eines Testbilder
(define Testbild
  (above (beside (square 10 "solid" "orchid")
                 (square 10 "solid" "aqua"))
         (beside (square 10 "solid" "gold")
                 (square 10 "solid" "magenta"))))

; Hilfsprozeduren für "rows-worker":
; - take
; - drop

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

; Prozedur "rows-worker" soll
; - eine natürliche Zahl w und eine Liste xs akzeptieren
; - übergebene Liste xs soll in eine Liste von Listen xss umgewandelt werden
; - jede innere Liste soll w Elemnte enthalten
; - letzte inner Lite kann weniger als w Elemente enthalten
(: rows-worker (natural (list-of %a) (list-of %a) -> (list-of (list-of %a))))

(check-expect (rows-worker 2
                          (list 1 2 3 4 5 6)
                          empty) (list (list 1 2)
                                       (list 3 4)
                                       (list 5 6)))
(check-expect (rows-worker 0 empty empty) empty)
(check-expect (rows-worker 2
                           (list 3)
                           empty) (list (list 3)))
(check-expect (rows-worker 2
                           (list 1 2 3 4 5)
                           empty) (list (list 1 2)
                                        (list 3 4)
                                        (list 5)))
(check-expect (rows-worker 3
                           (list 1 2 3)
                           empty) (list (list 1 2 3)))
(check-expect (rows-worker 3
                           (list 1 2 3 4 5)
                           empty) (list (list 1 2 3)
                                        (list 4 5)))

(define rows-worker
  (lambda (w xs acc)
    (cond
      ((empty? xs) acc)
      ((pair? xs) (make-pair (take w xs)
                             (rows-worker w (drop w xs) acc))))))

; Prozedur "rows" soll auf "rows-worker" zugreifen -> endrekursiv
(: rows (natural (list-of %a) -> (list-of (list-of %a))))

(check-expect (rows 2 (list 1 2 3 4 5)) (list (list 1 2)
                                              (list 3 4)
                                              (list 5)))
(check-expect (rows 3 (list 1 2 3)) (list (list 1 2 3)))
(check-expect (rows 1 empty) empty)
(check-expect (rows 3 (list "a" "b" "c" "d" "e")) (list (list "a" "b" "c")
                                                        (list "d" "e")))

(define rows
  (lambda (w xs)
    (rows-worker w xs empty)))

; schreiben einer Prozedur "flatten", die
; - eine Liste von Listen xss akzeptiert
; - eine Liste von Listen in eine Liste wandelt, die alle Elemente aus xss  enthält
(: flatten ((list-of (list-of %a)) -> (list-of %a)))

(check-expect (flatten empty) empty)
(check-expect (flatten (list (list 1))) (list 1))
(check-expect (flatten (list (list 1)
                             (list 2))) (list 1 2))
(check-expect (flatten (list (list #t)
                             (list 1 2 3)
                             (list "Baum" "Apfel"))) (list #t 1 2 3 "Baum" "Apfel"))
(check-expect (flatten (list (list 1 2 3)
                             (list)
                             (list 4 5))) (list 1 2 3 4 5))

(define flatten
  (lambda (xss)
   (cond 
    ((empty? xss) empty)
    ((pair? xss) (append (first xss) (flatten (rest xss)))))))

; Hilfsprozedur für "transpose":
; - every-nth
; - vert-sort

; Prozedur "every-nth" soll:
; - eine Liste xs und eine natürliche Zahl n akzeptieren
; - ein Liste zurückgeben
; - Liste enthält erstes und jedes weitere nte Element, also 1, 1+n, 1+2n, ...
; - n soll ungleich 0 sein
(: every-nth (natural (list-of %a) -> (list-of %a)))

(check-expect (every-nth 2 empty) empty)
(check-expect (every-nth 4 (list 1 2 3)) (list 1))
(check-expect (every-nth 2 (list 1 2 3 4 5)) (list 1 3 5))
(check-expect (every-nth 5 (list "a" "b")) (list "a"))
(check-expect (every-nth 1 (list 1 2 3)) (list 1 2 3))

(define every-nth
  (lambda (n xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs) (make-pair (first xs)
                             (every-nth n (drop (- n 1) (rest xs))))))))

; Prozedur "vert-sort" soll:
; - eine Liste von Listen xss akzeptiern
; - die Liste von Listen "vertikal" sortieren, also "transponieren" und dann in eine Liste ausgeben
; - innere Listen sollen dabei gleich lang sein
(: vert-sort ((list-of (list-of %a)) -> (list-of %a)))

(check-expect (vert-sort (list (list 1 2 3)
                               (list 4 5 6))) (list 1 4 2 5 3 6))
(check-expect (vert-sort empty) empty)
(check-expect (vert-sort (list (list "a" "c")
                               (list "b" "d"))) (list "a" "b" "c" "d"))

(define vert-sort
  (lambda (xss)
    (cond
      ((empty? xss) empty)
      ((pair? xss) (take (length (flatten xss)) (append (every-nth (length (first xss)) (flatten xss))
                                                        (vert-sort (rows (length (first xss)) (drop 1 (flatten xss))))))))))
                              
; schreiben einer Prozedur "transpose", die
; - eine Liste von Listen xss akzeptiert
; - die übergebene Liste "transponiert"
;   (bei 2-dimensionalen Matrix: Zeilen zu Spalten umwandelt)
; - innere Listen jeweils gleich lang
(: transpose ((list-of (list-of %a)) -> (list-of (list-of %a))))

(check-expect (transpose empty) empty)
(check-expect (transpose (list (list 1)
                               (list 2))) (list (list 1 2)))
(check-expect (transpose (list (list 4 5)
                               (list 1 2))) (list (list 4 1)
                                                  (list 5 2)))
(check-expect (transpose (list (list 1 2 3)
                               (list 4 5 6))) (list (list 1 4)
                                                    (list 2 5)
                                                    (list 3 6)))
(check-expect (transpose (list (list 1 2)
                               (list 3 4)
                               (list 5 6))) (list (list 1 3 5)
                                                  (list 2 4 6)))

(define transpose
  (lambda (xss)
    (cond
      ((empty? xss) empty)
      ((pair? xss) (rows (length xss) (vert-sort xss))))))

;================================================================================================

; Zugriff auf die Liste der Bildpunkte (Pixel) eines Bildes:  
; (: image->color-list (image -> (list-of rgb-color)))  
; (: color-list->bitmap ((list-of rgb-color) natural natural -> image))

; Prozedur "vert-mirror" soll Bild vertikal spiegeln
(: vert-mirror (image -> image))
(define vert-mirror
  (lambda (image)
    (color-list->bitmap (flatten (reverse (rows (image-width image) (reverse (image->color-list image)))))
                        (image-width image)
                        (image-height image))))

; Prozedur "horiz-mirror" soll Bild horizontal spiegeln
(: horiz-mirror (image -> image))
(define horiz-mirror
  (lambda (image)
    (color-list->bitmap (flatten (reverse (rows (image-width image) (image->color-list image))))
                        (image-width image)
                        (image-height image))))

; Prozedur "rotate-90-left" soll ein Bild um 90° gegen Uhrzeigersinn rotieren
(: rotate-90-left (image -> image))
(define rotate-90-left
  (lambda (image)
    (color-list->bitmap (flatten (reverse (transpose (rows (image-width image) (image->color-list image)))))
                        (image-width image)
                        (image-height image))))


; alle Testfälle nebeneinander
(define Lücke (square 10 "solid" "white"))

(beside Testbild
        Lücke
        (vert-mirror Testbild)
        Lücke
        (horiz-mirror Testbild)
        Lücke
        (rotate-90-left Testbild))