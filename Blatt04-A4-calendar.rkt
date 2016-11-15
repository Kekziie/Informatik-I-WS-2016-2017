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
(: calendar-date-day (calendar-date -> natural))
(: calendar-date-month (calendar-date -> natural))
(: calendar-date-year (calendar-date -> natural))

(check-expect (calendar-date-day birthday) 17)
(check-expect (calendar-date-month birthday) 9)
(check-expect (calendar-date-year birthday) 1995)

(define-record-procedures calendar-date
  make-calendar-date
  calendar-date?
  (calendar-date-day
   calendar-date-month
   calendar-date-year))

; mein Geburtstag
(: birthday calendar-date)

(define birthday
  (make-calendar-date 17 9 1995))

;(b)
; definiere richtiges Datum im Schaltjahr für Testfälle
; falsches Datum im normalen Jahr

(: date1 calendar-date)

(define date1
  (make-calendar-date 29 2 2020))

; definiere falsche Daten für Testfälle
(: false-date1 calendar-date)

(define false-date1
  (make-calendar-date 100 100 100))

(: false-date2 calendar-date)

(define false-date2
  (make-calendar-date 29 2 2001))

(: false-date3 calendar-date)

(define false-date3
  (make-calendar-date 35 1 1999))

(: false-date4 calendar-date)

(define false-date4
  (make-calendar-date 12 0 2013))

(: false-date5 calendar-date)

(define false-date5
  (make-calendar-date 0 4 1992))

; stellt fest, ob Monat korrekt ist

(: calendar-date-month-ok? (calendar-date -> boolean))

(check-expect (calendar-date-month-ok? birthday) #t)
(check-expect (calendar-date-month-ok? date1) #t)
(check-expect (calendar-date-month-ok? false-date1) #f)
(check-expect (calendar-date-month-ok? false-date2) #t)
(check-expect (calendar-date-month-ok? false-date3) #t)
(check-expect (calendar-date-month-ok? false-date4) #f)
(check-expect (calendar-date-month-ok? false-date5) #t)


(define calendar-date-month-ok?
  (lambda (date)
    (if (>= (calendar-date-month date) 1)
        (<= (calendar-date-month date) 12)
        #f)))

; stellt fest, ob Tage im Monat korrekt sind

(: calendar-date-day-ok? (calendar-date -> boolean))

(check-expect (calendar-date-day-ok? birthday) #t)
(check-expect (calendar-date-day-ok? date1) #f)
(check-expect (calendar-date-day-ok? false-date1) #f)
(check-expect (calendar-date-day-ok? false-date2) #f)
(check-expect (calendar-date-day-ok? false-date3) #f)
(check-expect (calendar-date-day-ok? false-date4) #t)
(check-expect (calendar-date-day-ok? false-date5) #f)

(define calendar-date-day-ok?
  (lambda (date)
    (if (>= (calendar-date-day date) 1)
        (if (<= (calendar-date-month date) 7)
            (if (= (modulo (calendar-date-month date) 2) 1)
                (<= (calendar-date-day date) 31)
                (if (= (calendar-date-month date) 2)
                    (<= (calendar-date-day date) 28)
                    (<= (calendar-date-day date) 30)))
            (if (= (modulo (calendar-date-month date) 2) 0)
                (<= (calendar-date-day date) 31)
                (<= (calendar-date-day date) 30)))
        #f)))

; stellt fest, ob Kalenderdatum-Record einem tatsächlichen Kalenderdatum entspricht
; Schaltjahre ignoriert

(: calendar-date-ok? (calendar-date -> boolean))

(check-expect (calendar-date-ok? birthday) #t)
(check-expect (calendar-date-ok? date1) #f)
(check-expect (calendar-date-ok? false-date1) #f)
(check-expect (calendar-date-ok? false-date2) #f)
(check-expect (calendar-date-ok? false-date3) #f)
(check-expect (calendar-date-ok? false-date4) #f)
(check-expect (calendar-date-ok? false-date5) #f)

(define calendar-date-ok?
  (lambda (date)
    (and (calendar-date-month-ok? date)
         (calendar-date-day-ok? date))))

; (c)
; stellt fest, ob Tage im Monat korrekt sind im Schaltjahr

(: calendar-date-day-ok/leap-year? (calendar-date -> boolean))

(check-expect (calendar-date-day-ok/leap-year? birthday) #t)
(check-expect (calendar-date-day-ok/leap-year? date1) #t)
(check-expect (calendar-date-day-ok/leap-year? false-date1) #f)
(check-expect (calendar-date-day-ok/leap-year? false-date2) #t)
(check-expect (calendar-date-day-ok/leap-year? false-date3) #f)
(check-expect (calendar-date-day-ok/leap-year? false-date4) #t)
(check-expect (calendar-date-day-ok/leap-year? false-date5) #f)

(define calendar-date-day-ok/leap-year?
  (lambda (date)
    (if (>= (calendar-date-day date) 1)
        (if (<= (calendar-date-month date) 7)
            (if (= (modulo (calendar-date-month date) 2) 1)
                (<= (calendar-date-day date) 31)
                (if (= (calendar-date-month date) 2)
                    (<= (calendar-date-day date) 29)
                    (<= (calendar-date-day date) 30)))
            (if (= (modulo (calendar-date-month date) 2) 0)
                (<= (calendar-date-day date) 31)
                (<= (calendar-date-day date) 30)))
        #f)))

; stellt fest, ob Kalenderdatum-Record einem tatsächlichen Kalenderdatum entspricht
; mit Schaltjahren

(: calendar-date-ok/leap-year? (calendar-date -> boolean))

(check-expect (calendar-date-ok/leap-year? birthday) #t)
(check-expect (calendar-date-ok/leap-year? date1) #t)
(check-expect (calendar-date-ok/leap-year? false-date1) #f)
(check-expect (calendar-date-ok/leap-year? false-date2) #f)
(check-expect (calendar-date-ok/leap-year? false-date3) #f)
(check-expect (calendar-date-ok/leap-year? false-date4) #f)
(check-expect (calendar-date-ok/leap-year? false-date5) #f)

(define calendar-date-ok/leap-year?
  (lambda (date)
    (if (= (modulo (calendar-date-year date) 4) 0)
        (and (calendar-date-day-ok/leap-year? date)
             (calendar-date-month-ok? date))
        (calendar-date-ok? date)))) 


     