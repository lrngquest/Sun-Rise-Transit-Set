(ns find-events.core
  (:import (java.time Instant ZoneOffset OffsetDateTime) )
  (:require [find-events.ssetc :as ss]  [find-events.ut2tdb :as  u]
            [find-events.efx   :as ef]  [find-events.dates :as da]
            [clojure.edn :as edn]       [clojure.java.io :as io]  )    
  (:gen-class))
(set! *warn-on-reflection* true)

(def dflt-obs {:longitude -1.6275195274847123  :latitude 0.7848169687565342
          :TZoffs -360    :TZ "US-CENTRAL"  :loc "MSP"} ) ;; TZoffs minutes

(defn fhm  ""  [v] (format "%2d:%02d" (:hour v) (:minute v)) )
(defn fmd "month day only"  [v] (format "%2d-%02d" (:month v) (:day v)) )
(defn fmdhm "" [v] (format "%s %s  " (fmd v) (fhm v) ))

;;2018    3-20 11:15    6-21  5:07    9-22 20:54   12-21 16:22
;;2018  Mar 20 11:15d   Jun 21 05:07d   Sep 22 20:54d   Dec 21 16:22

;;java -cp test3.jar:t0.jar org.shetline.skyviewcafe.CmdLnRST 2018 6 20
;;2018-06-20    5:26d   azi 125.206   13:15d   alt  68.469 (decl  23.435)   azi 234.741   21:03d

;;  y26-work % bb --classpath src:resources -m find-events.core 2026 1 2 3 5


(defn isLeapYear  " 1 ==> leap year"  [year]
  (cond
   (= 0 (mod year 400)) 1
   (= 0 (mod year 100)) 0
   (= 0 (mod year   4)) 1
   :else                0)  )


(defn printES "" [y  obs]
  (let [[se ss fe ws]  (ef/getEquinoxesAndSolsticesByYear y obs) ]
    (printf "\n%4d %s %s %s %s leap:%d  Dday: %s\n"
            (:year se) (fmdhm se) (fmdhm ss) (fmdhm fe) (fmdhm ws)
            (isLeapYear y)
            (["Sun""Mon""Tue" "Wed""Thu""Fri""Sat"]
             (->> (da/centuryAnchorDay (int (/ y 100.0))) ;; dDay inlined
                  (da/dDayForYear (mod y 100)  ))) )    )   )


(def s30  (/ 30.0 3600) )  ;; 30secs as an hour-fraction

(defn dlhm2 "subtract times using event vals" [ev1 ev0]  ;; Works, revised!
  (let [djd   (- (:val ev1) (:val ev0)) ;; day fraction
        hf    (/ (* 86400 djd) 3600.0)  ;; hours.fraction
        hfs30 (+ hf s30)  ;; adjust up by 30sec; NOTE int part is correct
        m1    (int (+ (* (- hf (int hf)) 60.0)  0.5))
        m2    (if (= m1 60)  0  m1)  ]   ;; minutes rounded & adjusted
 
    [ (int hfs30)  m2] )  )


(defn day-detail "with length of day" [y m d  obs]
  (let [[rz st]    (ef/getRiseAndSetTimes ss/SUN y m d  obs)
        tt         (ef/getTransitTimes ss/SUN y m d obs)
        [dlh dlm]  (dlhm2 st rz)  ]  ;; daylight hrs mins
    
    (printf "%2d-%02d %2d:%02d %2d:%02d %2d:%02d %10.5f  %9.5f (%9.5f)  %2dh %2dm\n"
            (:month rz) (:day rz)  (:hour rz) (:minute rz)  ;;(fmdhm rz)
            (:hour tt) (:minute tt) (:hour st )(:minute st) ;;(fhm tt) (fhm st)
            (ss/getLongitudeDeg (ss/getHorizontalPosition
                                 ss/SUN (:val rz) obs))
            (ss/getAltDeg (ss/getHorizontalPosition
                           ss/SUN (:val tt) obs))
            (ss/getLatitudeDeg (ss/getEquatorialPosition
                                ss/SUN (:val tt) obs ss/AbrrNut))
            dlh dlm   )    ;; dup m-day at EOL  Omit
     ) )


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


(declare print-daylight-progression vv  ESdetail)

(defn clix "enhanced cli -- short forms for frequent used cases" [va xobs]
  (case (count va)   ;; (empty)   y...
      0  (let [t   (.atOffset (Instant/now ) ZoneOffset/UTC)
               ny  (.getYear t)   nm (.getMonthValue t)   nd (.getDayOfMonth t)
               [y m d ]  (->>(- (da/getJD ny nm nd 12 0 0) 7) (da/getDate))  ]
           (daysByStepCount y m d 1 15  xobs)) ;; 2wk window around today's date

      2  (let [cy (va 0) ]  ;; assume  (va 0) year  (va 1)  0,1,?
           (when (= 0 (va 1))
             (doseq [m (range 1 13)] (day-detail  cy  m 20  xobs))
             (printES cy xobs)   (printf ":loc %s\n" (:loc xobs))  )
                     
           (when (= 1 (va 1))
             (print-daylight-progression cy xobs vv )
             (printf "\n\n" )
             (ESdetail cy xobs) )
           )
            
      3  (let [[y day weeks] va] ;; a-day-line-per-week  "almanac" case
           (daysByStepCount y 1 day 7 weeks  xobs)
           (println)  (printES y xobs)  ;; blank ln, ES info, :loc info
           (printf ":loc %s\n" (:loc xobs)) )
      
      4  (let [[y m d n] va] (daysByStepCount y m d 1 n  xobs) )
      
      (do (print " no args             ==> 2 week window around today's date\n"
                  "year 0                  ==> WG sun-declination diag. case\n"
                  "year 1                  ==> almanac p2 case"
                  "year start-day {52,53}  ==> almanac case\n"
                  "year month day number-of-days ==> general case\n") )   )
  )


(defn -main "reads 'config.edn' if present as  observer"  [& args]
  (let [va         (mapv read-string args)
        cfg-file   (io/file "config.edn")
        z-obs      (if (.exists cfg-file)
                     (edn/read-string (slurp cfg-file))
                     dflt-obs  ) ]    
    (clix va z-obs)
    ;;(printf ":loc %s\n" (:loc z-obs))
    )  )


(def vv [[1 8  12 2]  [2 5   11 4]  [2 26  10 14]  [3 17  9 25]
         [4 5   9 5]  [4 25  8 16]  [5 19   7 23]] )

(defn  print-daylight-progression "" [y obs vx]
  (printf"year %d   Approximate even hour day lengths\n" y)

  (doseq [mdmd vx]
    (let [[m1 d1 m2 d2]  mdmd
          [Lrz Lst]    (ef/getRiseAndSetTimes ss/SUN y m1 d1  obs)
          [rrz rst]    (ef/getRiseAndSetTimes ss/SUN y m2 d2  obs)
          [dLh1 dLm1]  (dlhm2 Lst Lrz)
          [dLh2 dLm2]  (dlhm2 rst rrz)       ]
      (printf "%2d-%02d  %2dh %02dm    %2dh %02dm  %2d-%02d\n"
              m1 d1  dLh1 dLm1  dLh2 dLm2  m2 d2) )  )  )  ;; let,doseq,fn


(def NUTATION 4)  (def HIGH_PRECISION 2)  (def ABERRATION 128)
(def hpflags (bit-or ABERRATION HIGH_PRECISION))
(def eqflags (bit-or ABERRATION NUTATION))

(defn calc-azi-alt "" [timeJDU obs ]
  (let [horPos  (ss/getHorizontalPosition ss/SUN timeJDU obs hpflags)
        aziDg0  (ss/getLongitudeDeg horPos)
        aziDeg  (if (< aziDg0 0.0 ) (+ 360 aziDg0)  aziDg0)
        
        altDeg  (ss/getAltDeg       horPos)
        
        equPos  (ss/getEquatorialPosition
                 ss/SUN (u/UT_to_TDB timeJDU)  obs eqflags)
        declDeg (ss/getDeclinationDeg  equPos)     ]
    ;;(printf " decl %f  azi %f  alt %f\n"  declDeg aziDeg altDeg)
    {:azi aziDeg  :alt altDeg  :decl declDeg}    )  )

(defn get-dlx "equinox event daylength" [y m d  es-h es-m  obs]
  (let [[rz st]    (ef/getRiseAndSetTimes ss/SUN y m d  obs)
        [dlh dlm]  (dlhm2 st rz) ]
    {:y y  :m m  :d d    :dlh dlh  :dlm dlm  :h es-h  :min es-m}    )  )

(defn prnt-etc "" [cmap xmap]
  ;;         mm-dd    hh:mm    evAZi  evALt  evDecL   dlH  dlM
  (printf "%2d-%02d %2d:%02d  %10.5f  %9.5f (%9.5f)  %2dh %2dm\n"
          (:m xmap) (:d xmap)   (:h xmap) (:min xmap)
          (:azi cmap) (:alt cmap) (:decl cmap)   (:dlh xmap) (:dlm xmap) )  )


(defn ESdetail "" [y obs]  
  (let [[se ss fe ws]  (ef/getEquinoxesAndSolsticesByYear y obs)
        cmap-se        (calc-azi-alt (:val se) obs)
        cmap-ss        (calc-azi-alt (:val ss) obs)
        cmap-fe        (calc-azi-alt (:val fe) obs)
        cmap-ws        (calc-azi-alt (:val ws) obs)
        xmap-se  (get-dlx y (:month se)(:day se) (:hour se)(:minute se) obs)
        xmap-ss  (get-dlx y (:month ss)(:day ss) (:hour ss)(:minute ss) obs)
        xmap-fe  (get-dlx y (:month fe)(:day fe) (:hour fe)(:minute fe) obs)
        xmap-ws  (get-dlx y (:month ws)(:day ws) (:hour ws)(:minute ws) obs)
        ]
    (printf "year %d   Equinox,solstice details\n" y)  ;; print year here once

    (prnt-etc cmap-se xmap-se )    (prnt-etc cmap-ss xmap-ss )
    (prnt-etc cmap-fe xmap-fe )    (prnt-etc cmap-ws xmap-ws )    ) )

