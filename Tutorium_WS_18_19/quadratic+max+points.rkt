;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname quadratic+max+points) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Aufgabe 1 quardratic

; quadriert eine Zahl a
(: sqr (real -> real))

(check-expect (sqr 2) 4)

(define sqr
  (lambda (a)
    (* a a)))

; gibt die Anzahl der Lösungen einer quadratischen Gleichung an
(: number-of-solutions (real real real -> (one-of 0 1 2)))

(check-expect (number-of-solutions 1 0 -1) 2)
(check-expect (number-of-solutions 2 4 2) 1)
(check-expect (number-of-solutions 1 0 1) 0)

(define number-of-solutions
  (lambda (a b c)
    (cond ((> (sqr b) (* 4 a c)) 2)
          ((= (sqr b) (* 4 a c)) 1)
          (else 0))))

; wählt das Maximum aus 2 Zahlen x,y aus
(: max-2 (real real -> real))

(check-within (max-2 2.3 1) 2.3 0.01)
(check-expect (max-2 2 4) 4)

(define max-2
  (lambda (x y)
    (if (> x y)
        x
        y)))

; wählt maximu aus 4 Zahlen a,b,c,d
(: max-4 (real real real real -> real))

(check-expect (max-4 1 2 3 4) 4)
(check-expect (max-4 1 2 4 4) 4)
(check-expect (max-4 1 4 3 0) 4)
(check-expect (max-4 4 2 3 -1) 4)

(define max-4
  (lambda (a b c d)
    (max-2 (max-2 a b) (max-2 c d))))

; berechnet Verwarngeld in € bei zu langem Parken in Überschreitungsdauer t in Minuten
(: zu-langes-Parken (real -> real))

(check-expect (zu-langes-parken 55) 10)
(check-expect (zu-langes-parken 30) 5)
(check-expect (zu-langes-parken 60) 10)
(check-expect (zu-langes-parken 0) 0)
(check-expect (zu-langes-parken 120) 15)
(check-expect (zu-langes-parken 180) 20)
(check-expect (zu-langes-parken 200) 25)

(define zu-langes-Parken
  (lambda (t)
    (cond ((= t 0) 0)
          ((<= t 30) 5)
          ((<= t 60) 10)
          ((<= t 120) 15)
          ((<= t 180) 20)
          (else 25))))

; berechnet Bußgeld bei einer roten Ampel
; Parameter : t wie lange Ampel schon rot ist in Sekunden, g Gefährdung, s Sachbeschädigung
(: rote-ampel-bußgeld (natural boolean boolean -> integer))

(check-expect (rote-ampel-bußgeld 1 #f #t) 125)
(check-expect (rote-ampel-bußgeld 0 #f #t) 0)
(check-expect (rote-ampel-bußgeld 1 #f #f) 50)
(check-expect (rote-ampel-bußgeld 1 #t #t) 125)
(check-expect (rote-ampel-bußgeld 15 #f #f) 125)
(check-expect (rote-ampel-bußgeld 15 #f #t) 200)
(check-expect (rote-ampel-bußgeld 2 #t #t) 200)

(define rote-ampel-bußgeld
  (lambda (t g s)
    (cond ((= t 0) 0)
          ((<= t 1)
           (if (or g s)
            125
            50))
          (else
           (if (or g s)
            200
            125)))))

; berechnet Punkte bei einer roten Ampel
; Parameter : t wie lange Ampel schon rot ist in Sekunden, g Gefährdung, s Sachbeschädigung
(: rote-ampel-punkte (natural boolean boolean -> natural))

(check-expect (rote-Ampel-punkte 1 #f #f) 3)
(check-expect (rote-Ampel-punkte 0 #t #t) 0)
(check-expect (rote-Ampel-punkte 1 #f #t) 4)
(check-expect (rote-Ampel-punkte 23 #t #t) 4)

(define rote-ampel-punkte
  (lambda (t g s)
    (cond ((= t 0) 0)
          ((and (= t 1) (not g) (not s)) 3)
          (else 4))))

; berechnet Fahrverbot bei einer roten Ampel
; Parameter : t wie lange Ampel schon rot ist in Sekunden, g Gefährdung, s Sachbeschädigung
(: rote-ampel-fahrverbot (real boolean boolean -> boolean))

(check-expect (rote-ampel-fahrverbot 15 #f #f) #t)
(check-expect (rote-ampel-fahrverbot 1 #f #f) #f)
(check-expect (rote-ampel-fahrverbot 0 #t #t) #f)
(check-expect (rote-ampel-fahrverbot 1 #t #t) #t)
(check-expect (rote-ampel-fahrverbot 13 #t #f) #t)

(define rote-ampel-fahrverbot
  (lambda (t g s)
    (cond ((= t 0) #f)
          ((and (= t 1) (not g) (not s)) #f)
          (else #t))))