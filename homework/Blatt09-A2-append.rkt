;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname Blatt09-A2-append) (read-case-sensitive #f) (teachpacks ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "universe.rkt" "teachpack" "deinprogramm") (lib "image2.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 2

; Äquivalenz beweisen mittels vollständiger Induktion
; P(xs) <=> (cat (take n xs) (drop n xs)) = xs für alle Listen und alle natürlichen Zahlen n

; Induktionsanfang: P(empty)
; ==========================

; (cat (take n empty) (drop n empty))

; => {Reduktion take, Reduktion drop}
; (cat empty empty)

; => {Reduktion cat}
; empty

; Induktionsschritt: ∀x∈M, xs∈M:(P(xs)=>P(make-pair x xs))
; =========================================================

; Fall n=0
; --------

; (cat (take 0 xs) (drop 0 xs))

; => {apply(λ) in take, apply(λ) in drop}
; (cat ((lambda 0 xs) (cond ((= 0 0) ...))) ((lambda 0 xs) (cond ((= 0 0) ...))))

; => {eval(cond) von take, eval(cond) von drop}
; (cat ((cond (#t empty)...) ((cond (#t xs)...)))

; => {Reduktion take, Reduktion drop}
; (cat empty xs)

; => {Reduktion cat}
; xs

; Fall n>0
; --------

; (cat (take n+1 xs) (drop n+1 xs))

; => {apply(λ) in take, apply(λ) in drop}
; (cat ((lambda n+1 xs) (cond ((= n+1 0) ...))    ((lambda n+1 xs) (cond ((= n+1 0) ...)))
;                             ((empty? xs) ...)                          ((empty? xs) ...)
;                             ((pair? xs) ...)))                         ((pair? xs) ...)))

; => {eval(cond) von take, eval(cond) von drop}
; (cat (cond (#f ...)                                  (cond (#f ...)
;            (#f ...)                                        (#f ...)
;            (#t (make-pair (first xs)                       (#t (drop (- n+1 1) (rest xs)))))
;                           (take (- n+1 1) (rest xs))))

; => {nach eval(cond) von take, nach eval(cond) von drop}
; (cat (make-pair (first xs)                   (drop (- n+1 1) (rest xs))))
;                 (take (- n+1 1) (rest xs)))

; => {apply(prim(-)) -> Annahme: - realisiert Differenz korrekt}
; (cat (make-pair (first xs)          (drop n (rest xs))))
;                 (take n (rest xs)))

; => {ersetzen, da (make-pair (first xs) (take n (rest xs))=(take n xs) und (drop n (rest xs))=(drop n xs)}
; (cat (take n xs) (drop n xs))

; => {Induktionsvoraussetzung: (cat (take n xs) (drop n xs)) = xs}
; xs

; Definitionen von cat, take und drop:

; Fügt zwei Listen xs und ys zu einer Listen zusammen
(define cat
  (lambda (xs ys)
    (cond
      ((empty? xs) ys)
      ((pair? xs) (make-pair (first xs)
                             (cat (rest xs) ys))))))

; Nimmt eine Liste xs entgegen und gibt die ersten n Elemente von xs zurück
(define take
  (lambda (n xs)
    (cond
      ((= n 0) empty)
      ((empty? xs) empty)
      ((pair? xs) (make-pair (first xs)
                             (take (- n 1) (rest xs)))))))

; Nimmt eine Liste xs entgegen und verwirft die ersten n Elemente dieser Liste
(define drop
  (lambda (n xs)
    (cond
      ((= n 0) xs)
      ((empty? xs) empty)
      ((pair? xs) (drop (- n 1) (rest xs))))))