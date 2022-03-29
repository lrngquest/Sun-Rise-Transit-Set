(ns find-events.core
  (:require [find-events.ssetc :as ss])
  (:require [find-events.efx   :as ef])  (:require [find-events.dates :as da])
  (:gen-class))


(def obs {:longitude -1.6275195274847123  :latitude 0.7848169687565342
          :TZoffs    -360    :TZ "US-CENTRAL"  :loc "MSP"} ) ;; TZoffs minutes 


(defn now [] (java.util.Date.))


(defn fymd "" [v] (format "%4d-%02d-%02d " (:year v)  (:month v) (:day v)) )
(defn fmdhm "" [v]
  (format "%2d-%02d %2d:%02d  " (:month v) (:day v) (:hour v) (:minute v) ))
(defn fhm  "" [v] (format "%2d:%02d " (:hour v) (:minute v)) )



(defn printES "" [y]
  (let [es  (ef/getEquinoxesAndSolsticesByYear y obs) ]
    (println (format "%4d  " (:year (es 0)) ) 
             (fmdhm (es 0))  (fmdhm (es 1))  (fmdhm (es 2))  (fmdhm (es 3)))  )
  )


;;java -cp test3.jar:t0.jar org.shetline.skyviewcafe.CmdLnRST 2018 6 20
;;2018-06-20    5:26d   azi 125.206   13:15d   alt  68.469 (decl  23.435)   azi 234.741   21:03d


(defn day-detail "" [y m d]
  (let [rs  (ef/getRiseAndSetTimes ss/SUN y m d  obs)
        tt  (ef/getTransitTimes ss/SUN y m d obs)  ]
    (println (fmdhm (rs 0)) (fhm tt) (fhm (rs 1))
             (format "  %10.5f  %9.5f (%9.5f)"
                     (ss/getLongitudeDeg (ss/getHorizontalPosition
                                         ss/SUN (:val (rs 0)) obs))
                     (ss/getAltDeg (ss/getHorizontalPosition
                                   ss/SUN (:val tt) obs))
                     (ss/getLatitudeDeg (ss/getEquatorialPosition
                                         ss/SUN (:val tt) obs ss/AbrrNut))
                     ;;could add set azi (...%9.5f)
;;(ss/getLongitudeDeg (ss/getHorizontalPosition ss/SUN (:val (rs 1)) obs))
                     ))) )

(defn WGdetail " 20th of month" [y m] (day-detail y m 20 ) )

(defn isLeapYear  " 1 ==> leap year"  [year]
  (cond
   (= 0 (mod year 400)) 1
   (= 0 (mod year 100)) 0
   (= 0 (mod year   4)) 1
   :else                0)
  )

(defn centuryAnchorDay "" [c]  (+ 2 (* 5 (mod (mod c 4) 7) ))  )

(defn dDayForYear  "day of week of dDay"  [T anchorDay]
  (let [T0 (if (odd?  T) (+ T  11) T)
        T1 (/ T0 2)
        T2 (if (odd? T1) (+ T1 11) T1 )
        T3 (- 7 (mod T2 7))  ]
    (mod (+ T3 anchorDay) 7) )
  )


(defn firstWed "DoM for 1st Wed" [y m]
  (let [ymDdaysDoW ([[0  3  7  7   4  2  6   4  1  5   3  7  5]
                     [0  4  1  7   4  2  6   4  1  5   3  7  5]] (isLeapYear y))
        anchor     (centuryAnchorDay (int (/ y 100.0)))
        DoWdDayYr  (dDayForYear (mod y 100) anchor)
        delta      [3 2 1 0 -1 -2 -3]
        propWDoM   (+ (ymDdaysDoW m)  (delta DoWdDayYr))     ]
    (cond (>  propWDoM 7)  (- propWDoM 7)
          (<= propWDoM 0)  (+ propWDoM 7)
          :else             propWDoM      )    )  )


(defn monthlyByDayStep "" [fmtFn y m ds step]
  (let [dim ([[0 31 28 31  30 31 30  31 31 30  31 30 31]
              [0 31 29 31  30 31 30  31 31 30  31 30 31]] (isLeapYear y))]
    (loop [d ds]  (fmtFn y m d)
          (if (> d (- (dim m) step))  0  (recur (+ d step)))  )))


(defn monthlyByDay "" [fmtFn y m]  (monthlyByDayStep fmtFn y m 1 1))


(defn monthlyByWkWed "" [fmtFn y m]
  (let [d (firstWed y m)] (monthlyByDayStep fmtFn y m d 7)) )


(defn yearlyByDay "" [fmtFn y]
  (doseq [m (range 1 13)] (monthlyByDay fmtFn y m )))


(defn yearlyByWeek "" [fmtFn y]
  (doseq [m (range 1 13)] (monthlyByWkWed fmtFn y m )))


(defn yearlyByMonth "" [ y]  (doseq [m (range 1 13)] (WGdetail  y m )))

(defn aDay "" [y m d] (day-detail y m d) )


(defn -main ""  [& args]
  (let [y  ((vec args) 0)]
    (printES y)
    (println)
    (yearlyByMonth y)
    (println)
    (yearlyByWeek day-detail y)
    (println)
    (monthlyByDay day-detail y 9)
    )
  
;;2018    3-20 11:15    6-21  5:07    9-22 20:54   12-21 16:22  

;;2018	Mar 20 11:15§	Jun 21 05:07§	Sep 22 20:54§	Dec 21 16:22
  
;;Ls-MacBook-Air:toys EdV$ ./planck -m dt.equinox 2018
;;2018
;;3-20 11:14:45    6-21 5:6:42    9-22 20:53:41    12-21 16:21:52

)
