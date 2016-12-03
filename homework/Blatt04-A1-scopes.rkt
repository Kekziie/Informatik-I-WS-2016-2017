;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt04-A1-scopes) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1

(define x 1)
(define y 5)

; i)
((lambda (x y)
   (+ (* 2 x) y))
   y x)

; 1) eval(id)
;    ((lambda (x y) (+ (* 2 x) y)) 5 x)

; 2) eval(id)
;    ((lambda (x y) (+ (* 2 x) y)) 5 1)

; 3) apply(λ)
;    (+ (* 2 5) 1)

; 4a) eval(id)
;     (+ (#<procedure:*> 2 5) 1)

; 4b) apply(prim (*))
;     (+ 10 1)

; 5a) eval(id)
;    (#<procedure:+> 10 1)

; 5b) apply(prim (+))
;     11

;ii)
((lambda (a b)
   (+ (* 2 x) y))
 x y)

; 1) eval(id)
;    ((lambda (a b) (+ (* 2 x) y)) 1 y)

; 2) eval(id)
;    ((lambda (a b) (+ (* 2 x) y)) 1 5)

; 3) apply(λ)
;    (+ (* 2 x) y)

; 4) eval(id)
;    (+ (* 2 1) y)

; 5a) eval(id)
;     (+ (#<procedure:*> 2 1) y)

; 5b) apply(prim (*))
;     (+ 2 y)

; 6) eval(id)
;    (+ 2 5)

; 7a) eval(id)
;     (#<procedure:+> 2 5)

; 7b) apply(prim (+))
;     7

; Lexikalische Bindung
; Im Programmtext muss man die Identifier <x> und <y> systematisch "von innen nach außen" zuordnen.
; Beispiele: i) als erstes muss man die hinteren Werte x und y durch ihren definierten Wert ersetzen 5 und 1
;               danach erfolgt apply(λ), d.h durch die Regelung der lexikalischen Bindung wird 5 in x eingesetzt,
;               da 5 der erste angegebene Wert ist, der eingesetzt werden muss und (* 2 x) die innere Klammer des Rumpfes ist
;               danach wird der Wert 1 in y eingsetzt, als 2. Wert der lambda-Abstraktion
;               dann wird der Rumpf (+ (* 2 5) 1) ausgewertet zu 11

;            ii) wie im ersten Bsp. werden die hinteren Wert durch ihre definierten Werte ersetzt
;                bei apply(λ) muss für a -> 1 und b -> 5 eingesetzt werden, da aber im Rumpf kein Parameter a oder b auftaucht, werden auch keine Parameter ersetzt,
;                d.h. der Rumpf bleibt unverändert bei (+ (* 2 x) y)
;                mithilfe von den definierten Werten, werden x und y ersetzt und dann der Rumpf ausgwertet zu 7

