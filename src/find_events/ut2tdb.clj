(ns find-events.ut2tdb (:require [find-events.const :as cn]) )

;; from o.s.a.UTConvertor

  ;; table for compatibility testing -- more current values available
  ;; from  http://stjarnhimlen.se/comp/time.html
;;Many simplifcations made
;; - recent years (i.e. after 1599) ==> span:1 in table values
;; - span:1 ==> tableMidYear : year
;; - above ==> n : 0  ==> dt2  is sole remaining term

(defn hDT "ala historicDeltaT" [y]
  (let [iy   (int y)     yx   (if (> iy 2014) 0  (- iy 2000))  ]
    ([63.82 64.09 64.30 64.47 64.57 64.68 64.84  65 65 66 66 66 67 69 71] yx)) )

(defn getDeltaTatJulianDate "" [ timeJDE]
  (let [year  (+ (/ (- timeJDE cn/JD_J2000) 365.25) 2000.0)
        t     (/ (- year 2000) 100.0)
        dta   (+ 102.0 (* 102.0 t) (* 25.3 t t))
        dt3   (if (and (> year 2014) (< year 2100)) ;;off-end of table
                (+ dta (* 0.5161 (- year 2100.0)) )
                (hDT year) )  ]
   dt3)
  )


(defn UT_to_TDB "" [timeJDU]
  (reduce
   (fn [timeJDE v]
     (+ timeJDU (/ (getDeltaTatJulianDate timeJDE) 86400.0))   )
   timeJDU  (range 5) )    )

