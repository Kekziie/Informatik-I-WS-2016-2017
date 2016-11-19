;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt05-A1-game) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 1

; Ausschnitt eines Computerspiels mit:
; - Spielfigur (character) besteht aus: Name, ganzzahliger Gesundheitszustand (0-100) und zweidimensionale Position
; - Bomben (bomb) bestehen aus: Detonationsradius ("blast radius") und Schadenswert
; - Bombenangriff (attack) besteht aus: Position und Bombe

; Treffer bei Bombenangriff: Abstand zur Bombe kleiner als Detonationsradius
;                            Gesundheitszustand verringert

; Schaden der Bombe: verringert bei zunehmender Entfernung des Detonationsradiuses
;                    Reduzieung des Gesundheitszustand um folgenden Wert: (1 - D/R) * S
;                    wobei D Distanz, R Detonationsradius (d<r) und S Schadenswert sind

; Spielfigur: erleidet keinen Schaden, wenn nicht im Detonationsradius
;             Gesundheitszustand darf nicht kleiner als 0 sein