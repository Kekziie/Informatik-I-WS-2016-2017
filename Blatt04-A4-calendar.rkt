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
(check-property
 (for-all ((day natural)
           (month natural)
           (year natural))
   (and (= (calendar-date-day (make-calendar-date day month year)) day)
        (= (calendar-date-month (make-calendar-date day month year)) month)
        (= (calendar-date-year (make-calendar-date day month year)) year))))
           

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

(: false-date6 calendar-date)

(define false-date6
  (make-calendar-date 31 11 2000))

; stellt fest, ob Monat korrekt ist, also zwischen 1 und 12

(: month-ok? (calendar-date -> boolean))

(check-expect (month-ok? birthday) #t)
(check-expect (month-ok? date1) #t)
(check-expect (month-ok? false-date1) #f)
(check-expect (month-ok? false-date2) #t)
(check-expect (month-ok? false-date3) #t)
(check-expect (month-ok? false-date4) #f)
(check-expect (month-ok? false-date5) #t)
(check-expect (month-ok? false-date5) #t)


(define month-ok?
  (lambda (date)
    (if (>= (calendar-date-month date) 1)
        (<= (calendar-date-month date) 12)
        #f)))

; stellt fest, ob Tage im Monat korrekt sind

(: day-ok? (calendar-date -> boolean))

(check-expect (day-ok? birthday) #t)
(check-expect (day-ok? date1) #f)
(check-expect (day-ok? false-date1) #f)
(check-expect (day-ok? false-date2) #f)
(check-expect (day-ok? false-date3) #f)
(check-expect (day-ok? false-date4) #t)
(check-expect (day-ok? false-date5) #f)
(check-expect (day-ok? false-date6) #f)

(define day-ok?
  (lambda (date)
         ; kontolliert, dass Tag nicht 0 sein darf 
    (if (>= (calendar-date-day date) 1)
            ; schaut, ob man sich in der Hälfte des Jahres befindet,
            ; da Juli + August beide 31 Tage besitzen
        (if (<= (calendar-date-month date) 7)
            ; 1.Monathälfte
                ; gibt jedem "ungeraden" Monat (1 3 5 7) 31 Tage
            (if (= (modulo (calendar-date-month date) 2) 1)
                (<= (calendar-date-day date) 31)
                    ; gibt dem 2.Monat 28 Tage
                    ; gibt jedem "geraden" Monat (4 6) 30 Tage
                (if (= (calendar-date-month date) 2)
                    (<= (calendar-date-day date) 28)
                    (<= (calendar-date-day date) 30)))
             ; 2.Monathälfte
                 ; gibt jedem geraden Monat (8 10 12) 31 Tage
                 ; gibt jedem ungeraden Monat (9 11) 30 Tage
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
(check-expect (calendar-date-ok? false-date6) #f)

(define calendar-date-ok?
  (lambda (date)
    (and (month-ok? date)
         (day-ok? date))))

; (c)
; stellt fest, ob Tage im Monat korrekt sind im Schaltjahr

(: day-ok/leap-year? (calendar-date -> boolean))

(check-expect (day-ok/leap-year? birthday) #t)
(check-expect (day-ok/leap-year? date1) #t)
(check-expect (day-ok/leap-year? false-date1) #f)
(check-expect (day-ok/leap-year? false-date2) #t)
(check-expect (day-ok/leap-year? false-date3) #f)
(check-expect (day-ok/leap-year? false-date4) #t)
(check-expect (day-ok/leap-year? false-date5) #f)

(define day-ok/leap-year?
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
        (and (day-ok/leap-year? date)
             (month-ok? date))
        (calendar-date-ok? date)))) 
     