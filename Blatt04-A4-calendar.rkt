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

; (b)
; stellt fest, ob Monat korrekt ist

(define calendar-date-month-ok?
  (lambda (date)
    (if (>= (calendar-date-month date) 1)
        (<= (calendar-date-month date) 12)
        #f)))

; stellt fest, ob Tage im Monat korrekt sind

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

(define calendar-date-ok?
  (lambda (date)
    (and (calendar-date-month-ok? date)
         (calendar-date-day-ok? date))))

; (c)
; stellt fest, ob Tage im Monat korrekt sind im Schaltjahr

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
; mit Schaltjahre 

(define calendar-date-ok/leap-year?
  (lambda (date)
    (if (= (modulo (calendar-date-year date) 4) 0)
        (and (calendar-date-day-ok/leap-year? date)
             (calendar-date-month-ok? date))
        (calendar-date-ok? date)))) 


     