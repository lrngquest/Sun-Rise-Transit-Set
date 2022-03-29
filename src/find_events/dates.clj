(ns find-events.dates)

(defn getDate
  " julian date to year,month,day,hour,minute,second UTC --  Meeus ch. 7"
  [jd ]
  (let [ Z (Math/floor (+ jd 0.5))
        F (- (+ jd 0.5) Z)
        A1 Z
        a (int (/ (- Z 1867216.25) 36524.25))
        A2 (+ A1 1 a  (/ a -4))
        A (if (>= Z 2299161)  A2  A1)

        B (+ A 1524)
        C (int (/ (- B 122.1) 365.25))
        D (int (* C 365.25))
        E (int (/ (- B D) 30.6001))
        exactDay    (- (- (+ F B) D) (int (* 30.6001 E)) )
        day (int exactDay)
        month (int ( if (< E 14) (- E 1) (- E 13)))
        year1 (- C 4715)
        year (if (> month 2)  (dec year1) year1)
        
        h (/ (* (- exactDay day) 86400) 3600.0) ; SECONDS_PER_DAY
        hour (int h)
        m (* (- h hour) 60.0)
        minute (int m)
        second (int (* (- m minute) 60.0) ) ]
      [year month day  hour minute second]
    )
  )


(defn getJD  "extr func (T. Alonso Albi)"    [y m d h mn s]
  (let [Y (if (< m 3) (dec y) y)
        M (if (< m 3) (+ m 12) m)
        A (int (/ Y 100))
        B (+ (- 2 A)  (/ A 4))
        dayFraction (/ (+ h (/ (+ mn (/ s 60.0)) 60.0)) 24.0)
        jd  (+ dayFraction
               (int (* 365.25 (+ Y 4716)))
               (int (* 30.6001 (+ M 1)))
               d  B  -1524.5)
        ]
    jd)   ;;Assert jd not in 2299150.0..2299160.0 !
  )



;; finding first-Sunday in Nov as end of DST  (US post-2007 rules)
;; finding second-Sunday in Mar as start of DST by subtraction

;;borrow some code from  cal.clj
(defn centuryAnchorDay  ""  [c]   (+ 2 (* 5 (mod (mod c 4) 7) ))  )

(defn dDayForYear  "day of week of dDay"  [T anchorDay]
  (let [T0 (if (odd?  T) (+ T  11) T)
        T1 (/ T0 2)
        T2 (if (odd? T1) (+ T1 11) T1 )
        T3 (- 7 (mod T2 7))  ]
    (mod (+ T3 anchorDay) 7) )
  )

;; We know Dday-for-Nov _always_ 7th  (9-to-5 at the 7-11) regardless
;; of leap-year.
;; We assert day-of-month-DST-end:  (- 7 dayOfWkDayForYear)

(defn getDSTntrvl "" [year ]
  (let [DomDstEnd (->> (centuryAnchorDay (int (/ year 100.0)))
                       (dDayForYear (mod year 100) )
                       (- 7 ) )
        EndJD      (getJD year 11 DomDstEnd  0 0 0)
        [yr mn da h m s]  (getDate (- EndJD 238))   ] ;;aka 34weeks
    {:yr yr  :stMn mn  :stDy da  :enMn 11  :enDy DomDstEnd} ) )


(defn getDSTfacts "" [year month day  dstM]
  (let [{:keys [yr stMn stDy enMn enDy] }  dstM ] ;;map de-structure
    (cond
     (and (= year yr) (< month stMn)            )    [ 0   1440] 
     (and (= year yr) (= month stMn) (= day stDy))   [60.0 1500]
     (and (= year yr) (= month stMn) (> day stDy))   [60.0 1440]
     (and (= year yr) (> month stMn) (< month enMn)) [60.0 1440]
     (and (= year yr) (= month enMn) (< day enDy))   [60.0 1440]
     (and (= year yr) (= month enMn) (= day enDy))   [ 0   1380]
     :else                                           [ 0   1440] ))  )
