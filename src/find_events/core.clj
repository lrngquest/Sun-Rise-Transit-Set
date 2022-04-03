(ns find-events.core
  (:import (java.time Instant ZoneOffset OffsetDateTime) )
  (:require [find-events.ssetc :as ss]
            [find-events.efx   :as ef]  [find-events.dates :as da]
            [clojure.edn :as edn]       [clojure.java.io :as io]  )    
  (:gen-class))
(set! *warn-on-reflection* true)


(def dflt-obs {:longitude -1.6275195274847123  :latitude 0.7848169687565342
          :TZoffs -360    :TZ "US-CENTRAL"  :loc "MSP."} ) ;; TZoffs minutes

(defn fmdhm "" [v]
  (format "%2d-%02d %2d:%02d  " (:month v) (:day v) (:hour v) (:minute v) ))
(defn fhm  "" [v] (format "%2d:%02d " (:hour v) (:minute v)) )


;;2018    3-20 11:15    6-21  5:07    9-22 20:54   12-21 16:22
;;2018	Mar 20 11:15d	Jun 21 05:07d	Sep 22 20:54d	Dec 21 16:22


(defn printES "" [y  obs]
  (let [es  (ef/getEquinoxesAndSolsticesByYear y obs) ]
    (println (format "%4d  " (:year (es 0)) ) 
             (fmdhm (es 0))  (fmdhm (es 1))  (fmdhm (es 2))  (fmdhm (es 3)))  )
  )


;;java -cp test3.jar:t0.jar org.shetline.skyviewcafe.CmdLnRST 2018 6 20
;;2018-06-20    5:26d   azi 125.206   13:15d   alt  68.469 (decl  23.435)   azi 234.741   21:03d

(defn dl-hm "subtract times, borrowing as needed" [ev1 ev0]
  (let [h1  (:hour ev1)   m1  (:minute ev1)
        h0  (:hour ev0)   m0  (:minute ev0) ]
    (if (>= m1 m0)  [(- h1 h0) (- m1 m0)]    [(- h1 1 h0) (- (+ m1 60) m0)]) )  )


(defn day-detail "with length of day" [y m d  obs]
  (let [rs  (ef/getRiseAndSetTimes ss/SUN y m d  obs)
        tt  (ef/getTransitTimes ss/SUN y m d obs)
        [dlh dlm]  (dl-hm (rs 1) (rs 0))  ]  ;; daylight hrs mins
    (println (fmdhm (rs 0)) (fhm tt) (fhm (rs 1))
             (format " %10.5f  %9.5f (%9.5f)  %2dh %2dm"
                     (ss/getLongitudeDeg (ss/getHorizontalPosition
                                         ss/SUN (:val (rs 0)) obs))
                     (ss/getAltDeg (ss/getHorizontalPosition
                                   ss/SUN (:val tt) obs))
                     (ss/getLatitudeDeg (ss/getEquatorialPosition
                                         ss/SUN (:val tt) obs ss/AbrrNut))
                     ;;could add set azi (...%9.5f)
;;(ss/getLongitudeDeg (ss/getHorizontalPosition ss/SUN (:val (rs 1)) obs))
                     dlh dlm))) )


(defn isLeapYear  " 1 ==> leap year"  [year]
  (cond
   (= 0 (mod year 400)) 1
   (= 0 (mod year 100)) 0
   (= 0 (mod year   4)) 1
   :else                0)
  )


(defn daysByStepCount [y m d stsize no  obs]
  (let [dim ([[0 31 28 31  30 31 30  31 31 30  31 30 31]
              [0 31 29 31  30 31 30  31 31 30  31 30 31]] (isLeapYear y))]
    (reduce (fn [[y m d] days]
              (day-detail y m d  obs)
  ;; incrementing parts of date with carry-out as needed
              (let [std          (+ d stsize)
                    m-end        (dim m)
                    [lmda coda]  (if (> std m-end)  [(- std m-end) 1]  [std 0])
                    smn          (+ m coda)
                    [lmmn comn]  (if (> smn 12)     [1 1]              [smn 0])
                    styr         (+ y comn)                               ]
                [styr lmmn lmda] )  )
          [y m d]  (range no) )   )  )


(defn dDay "simplified " [year]
  (->> (da/centuryAnchorDay (int (/ year 100.0)) )
       (da/dDayForYear (mod year 100) ) )  )


(defn clix "enhanced cli -- short forms for frequent used cases" [va xobs]
  (case (count va)   ;; (empty)   y...
      0  (let [t   (.atOffset (Instant/now ) ZoneOffset/UTC)
               ny  (.getYear t)   nm (.getMonthValue t)   nd (.getDayOfMonth t)
               [y m d ]  (->>(- (da/getJD ny nm nd 12 0 0) 7) (da/getDate))  ]
           (daysByStepCount y m d 1 15  xobs)) ;; 2wk window around today's date
      
      2  (when (= 0 (va 1)) ;; ala WG sun-decln. diag.
           (doseq [m (range 1 13)] (day-detail  (va 0) m 20  xobs))
           (println ) (printES (va 0) xobs)  )
      
      
      3  (do ;; yr day {52,53}  one-day-per-week  "almanac" case
           (daysByStepCount (va 0) 1 (va 1) 7 (va 2)  xobs)  (println)
           (printES (va 0) xobs)
           (printf "\nleap-year: %d   Dday: %s\n"  (isLeapYear (va 0))
                   (["Sun""Mon""Tue" "Wed""Thu""Fri""Sat"](dDay (va 0)))) )
      
      4  (daysByStepCount (va 0) (va 1) (va 2) 1  (va 3)  xobs)

      
      (do (print " no args                 ==>  2 week window around today's date\n"
                  "year 0                  ==>  WG sun-declination diag. case\n"
                  "year start-day {52,53}  ==>  almanac case\n"
                  "year month day number-of-days ==> general case\n") )   )
  )


(defn -main "reads 'config.edn' if present as  observer"  [& args]
  (let [va         (mapv read-string args)
        cfg-file   (io/file "config.edn")
        z-obs      (if (.exists cfg-file)
                     (edn/read-string (slurp cfg-file))
                     dflt-obs  ) ]    
    (clix va z-obs)
    (printf ":loc %s\n" (:loc z-obs)) )  )
