;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt04-A4-calendar) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Aufgabe 4

; (a)
; Ein Kalenderdatum (calendar-date) besteht aus
; - Tag (day)
; - Monat (month)
; - Jahr (year)

(: make-calendar-date (natural natural natural -> calendar-date))

(define-record-procedures calendar-date
  make-calendar-date
  calendar-date?
  (calendar-date-day
   calendar-date-month
   calendar-date-year))