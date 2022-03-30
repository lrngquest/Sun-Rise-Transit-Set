(ns find-events.core
  (:require [find-events.ssetc :as ss])
  (:require [find-events.efx   :as ef])  (:require [find-events.dates :as da])
  (:gen-class))


(def obs {:longitude -1.6275195274847123  :latitude 0.7848169687565342
          :TZoffs    -360    :TZ "US-CENTRAL"  :loc "MSP"} ) ;; TZoffs minutes 


(defn fymd "" [v] (format "%4d-%02d-%02d " (:year v)  (:month v) (:day v)) )
(defn fmdhm "" [v]
  (format "%2d-%02d %2d:%02d  " (:month v) (:day v) (:hour v) (:minute v) ))
(defn fhm  "" [v] (format "%2d:%02d " (:hour v) (:minute v)) )


;;2018    3-20 11:15    6-21  5:07    9-22 20:54   12-21 16:22
;;2018	Mar 20 11:15d	Jun 21 05:07d	Sep 22 20:54d	Dec 21 16:22


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


(defn isLeapYear  " 1 ==> leap year"  [year]
  (cond
   (= 0 (mod year 400)) 1
   (= 0 (mod year 100)) 0
   (= 0 (mod year   4)) 1
   :else                0)
  )

(defn firstWed "DoM for 1st Wed" [y m]
  (let [ymDdaysDoW ([[0  3  7  7   4  2  6   4  1  5   3  7  5]
                     [0  4  1  7   4  2  6   4  1  5   3  7  5]] (isLeapYear y))
        anchor     (da/centuryAnchorDay (int (/ y 100.0)))
        DoWdDayYr  (da/dDayForYear (mod y 100) anchor)
        delta      [3 2 1 0 -1 -2 -3]
        propWDoM   (+ (ymDdaysDoW m)  (delta DoWdDayYr))     ]
    (cond (>  propWDoM 7)  (- propWDoM 7)
          (<= propWDoM 0)  (+ propWDoM 7)
          :else             propWDoM      )    )  )


(defn monthlyByDayStep "" [fmtFn y m ds step]
  (let [dim ([[0 31 28 31  30 31 30  31 31 30  31 30 31]
              [0 31 29 31  30 31 30  31 31 30  31 30 31]] (isLeapYear y))]
    (reduce (fn [d v]
              (fmtFn y m d)
              (if (> d (- (dim m) step))  (reduced 0) (+ d step) )
              )
            ds (range 1 32))   ))

(defn monthlyByDay "" [fmtFn y m]  (monthlyByDayStep fmtFn y m 1 1))


(defn monthlyByWkWed "" [fmtFn y m]
  (let [d (firstWed y m)] (monthlyByDayStep fmtFn y m d 7)) )


(defn yearlyByDay "" [fmtFn y]
  (doseq [m (range 1 13)] (monthlyByDay fmtFn y m )))


(defn yearlyByWeek "" [fmtFn y]
  (doseq [m (range 1 13)] (monthlyByWkWed fmtFn y m )))


(defn yearlyByMonth "" [ y]  (doseq [m (range 1 13)] (day-detail  y m 20 )))


(defn daysByCount [y m d no]
  (let [dim ([[0 31 28 31  30 31 30  31 31 30  31 30 31]
              [0 31 29 31  30 31 30  31 31 30  31 30 31]] (isLeapYear y))]
    (reduce (fn [[y m d] days]
            (day-detail y m d)
            (if (> (inc d) (dim m))
              (if (= 12 m)  [(inc y) 1 1]   [y (inc m) 1]);; y,m rollover resp
              [y m (inc d)] ) )  ;;else advance d only
          [y m d]  (range no)  )   )  )


(defn -main ""  [& args]
  (let [va (vec (map read-string args)) ] ;; y   y m   y m d
    (case (count va)
      1  (do (println (va 0))  (yearlyByDay day-detail (va 0)) )
      
      2  (cond
          (> (va 1)0)
          (do (println (va 0))  (monthlyByDay day-detail (va 0) (va 1)) )
          
          (= 0 (va 1))
          (do (printES (va 0) ))  )
      
      3  (cond
          (> (va 1)0)
          (do (println (va 0))  (day-detail (va 0) (va 1) (va 2)) )
          
          (and (= 0 (va 1)) (= 0 (va 2)))
          (do (println (va 0))  (yearlyByWeek day-detail (va 0)))
          (and (= 0 (va 1)) (= 1 (va 2)))
          (do (println (va 0))  (yearlyByMonth (va 0))) ) ;;WG

      4  (do (daysByCount (va 0) (va 1) (va 2)  (va 3)) )   )    )  )
