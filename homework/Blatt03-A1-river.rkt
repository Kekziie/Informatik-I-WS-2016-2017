;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt03-A1-river) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1

; Beschreibung der Aufgabe
; Schiff möchte Fluss überqueren ohne den Wasserfall hinunter zu fallen
; Flussbreite x = 400 m
; Flusslänge (vom Schiff bis zum Wasserfall) y = 200 m
; Geschwindigkeit vSchiff = 5 m/s
; Geschw. vFluss = 1.5 m/s


; Aufgabe 1 (a) i)

; gibt an #t Sturz und #f stürzt nicht
; bei einer gegebenen Geschwindigkeit vFluss und vSchiff in m/s

(: plunges? (real real -> boolean))

(check-expect (plunges? 0.5 1.5) #t)
(check-expect (plunges? 10 5) #f)
(check-expect (plunges? 400 5) #f)

(define plunges?
  (lambda (vFluss vSchiff)
    (>= 200
        (* vFluss
           (/ 400
              vSchiff)))))


; Aufgabe 1 (a) ii)

; rechnet Abtreibung in Flussrichtung in m Meter
; angeben von Geschwindigkeit vSchiff (in m/s), Geschw. vFluss (in m/s) und gefahrene Strecke s (in m)
(: drift (real real real -> real))

(check-within (drift 5 1.5 0) 0 0.01)
(check-within (drift 80 4 1.5) 30 0.01)
(check-within (drift 50.5 4.5 3.5) 39.28 0.01)
(check-within (drift 100 1.5 5) 333.33 0.01)

(define drift
  (lambda (s vSchiff vFLuss)
      (* vFluss
         (/ s
            vSchiff))))

; Aufgabe 1 (b) i)

; definiere Größe des Flusses
(define Flussform (rectangle 500 300 "solid" "aqua"))

; definiere Größe der Uferform
(define Uferform (rectangle 50 300 "solid" "dark green"))

; Schiff bauen aus verschiedenen Teilen

(define Rumpf (rectangle 20 10 "solid" "brown"))

(define Schiff-Spitze (triangle 10 "solid" "brown"))

(define Schiff-hinten (circle 5 "solid" "brown"))

(define Schiffform (overlay/xy (beside Rumpf
                                       (rotate -90 Schiff-Spitze))
                               -5 0
                               Schiff-hinten))

; Bau eines Steges

(define Stegform1 (rectangle 25 5 "solid" "saddlebrown"))

(define Stegform2 (rectangle 3 6 "solid" "saddlebrown"))

(define Stegform-links (overlay/xy Stegform1
                                   13 5
                                   Stegform2))

(define Stegform-rechts (overlay/xy Stegform1
                                    6 -5
                                    Stegform2))

; Weg zum Steg:
; erstelle Kurven des Wegs

(define kurve1 (ellipse 25 10 "solid" "chocolate"))
(define kurve2 (ellipse 25 10 "solid" "dark green"))

(define rechtskurve (overlay/xy kurve2
                                0 5
                                kurve1))

(define linkskurve (overlay/xy kurve2
                               0 -5
                               kurve1))

(define wegmitte (rotate -40 (rectangle 10 5 "solid" "chocolate")))

; erstelle Weg
(define Weg (overlay/xy wegmitte
                        -20 -2.5
                        (overlay/xy linkskurve
                                    25 0
                                    rechtskurve)))

; erstelle Ufer mit Weg zum Steg

(define Ufer-links
  (overlay/xy Weg
              -1 -75
             (overlay/xy Stegform-links
                         -50 -80
                         Uferform)))


(define Ufer-rechts
  (overlay/xy Stegform-rechts
              25 -220
             (overlay/xy Weg
                         0 -222
                         Uferform)))
              
; definieren einer Bildgröße
(define scene (rectangle 500 300 "outline" "white"))

; Aufgabe 1 (b) ii)
; Schiff soll vom linken Ufer zum rechten Ufer fahren

(: crossing (natural -> image))

(define crossing
  (lambda (t)
    (clear-pinhole
     (overlay/pinhole (Ufer t)
                      (boat-at t)
                      (Fluss t)))))

(define Ufer
  (lambda (t)
    (overlay/xy Ufer-rechts
                -425 0
               (overlay/xy Ufer-links
                           0 0
                           scene))))

(define Fluss
  (lambda (t)
    (underlay/xy Flussform
                 0 0
                 scene)))

; gibt Position des Schiffes an zum Zeitpunk t

(: boat-at (natural -> image))
(define boat-at
  (lambda (t)
    (put-pinhole (move-boat 200 -165 t)
                 (move-boat 60 -60 t) 
                 Schiffform)))

(: move-boat (integer integer natural -> integer))

(check-expect (move-boat -10 10 0) -10)
(check-expect (move-boat -10 10 100) 10)
(check-expect (move-boat -10 10 50) 0)
(check-expect (move-boat -10 10 101) 10)

(define move-boat
  (lambda (x0 x1 t)
    (+ x0
       (floor (* (min t
                      100)
                 (/ (- x1 x0)
                      100))))))

; Szene vom Zeitpunkt t=0
; Schiff am linken Ufer
(crossing 0)

; Szene vom Zeitpunkt t=100
; Schiff am rechten Ufer
(crossing 100)

; Animation von Schifffahrt vom linkem Steg zum rechten Steg
(animate crossing)

