;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt02-A3-fuel) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
;(a.)
;Benzinverbrauch in Liter pro 100km
; wie weit kann man mit einer bestimmten Benzinmenge fahren?

(: liters-per-hundred-kilometers (real real -> real))

(check-expect (liters-per-hundred-kilometers 3 30)10)
(check-expect (liters-per-hundred-kilometers 99 99)100)
(check-expect (liters-per-hundred-kilometers 10 5)200)
(check-expect (liters-per-hundred-kilometers 1/2 1/4)200)
(check-within (liters-per-hundred-kilometers 2.5 10.5)23.8 0.1)

(define liters-per-hundred-kilometers
  (lambda (L km) (/ (* L 100)
                    km)))

;(b.)
;Reichweite in Meilen pro Gallone Benzinverbrauch

(: miles-per-gallon (real real -> real))

(check-expect (miles-per-gallon 30 3)10)
(check-expect (miles-per-gallon 3 3)1)
(check-within (miles-per-gallon 0.5 10.5) 0.05 0.1)

(define miles-per-gallon
  (lambda (mi gal) (/ mi gal)))

;(c.)
;umrechnen von Kilometern in Meilen
;umrechnen von Meilen in Kilometern

(: kilometers->miles (real -> real))
(: miles->kilometers (real -> real))

(check-within (kilometers->miles 10) 6.2 0.1)
(check-within (kilometers->miles 2.5) 1.5 0.1)

(check-within (miles->kilometers 10) 16.1 0.1)
(check-within (miles->kilometers 2.5) 4 0.1)

(define kilometers-per-mile 1.61)

(define kilometers->miles
  (lambda(km) (/ km kilometers-per-mile)))

(define miles->kilometers
  (lambda (mi) (* mi kilometers-per-mile)))

;(d.)
; umrechnen von Liter (L) in Gallonen (gal)
; umrechnen von Gallonen (gal) in Liter (L)

(: liters->gallons (real -> real))
(: gallons->liters (real -> real))

(check-within (liters->gallons 10) 2.64 0.1)
(check-within (liters->gallons 2.5) 0.7 0.1)

(check-within (gallons->liters 10) 37.9 0.1)
(check-within (gallons->liters 2.5) 9.5 0.1)

(define liters-per-gallons 3.79)

(define liters->gallons
  (lambda (L) (/ L liters-per-gallons)))

(define gallons->liters
  (lambda (gal) (* gal liters-per-gallons)))

;(e.)
;umrechnen von Benzinverbrauch (BV)l/100km in Reichweite mi/gal

;(: l/100km->mi/gal (real->real))

;(check-expect (l/100km->mi/gal 10) 23)

;(define l/100km->mi/gal
;  (lambda (BV)) (/(kilometers->miles(/(* l 100)
;                                      BV))
;                  (liters->gallons (/(* BV km)
;                                      100))))

;(f.)
;umrechnen von Reichweite (R) mi/gal in Benzinverbrauch (BV)

;(:mi/gal->l/100km (real -> real))

;(check-expect (mi/gal->l/100km 50) 4)

;(define mi/gal->l/100km
;  (lambda (R) (/ (* 100
;                    (gallons->liters (/mi R)
;                                     (miles->kilometers (* R gal)))))))
