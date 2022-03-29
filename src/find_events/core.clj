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

(defn monthly ""
  ([y m]
     (let [dim ([0  31 28 31  30 31 30  31 31 30  31 30 31] m) ]
       (monthly y m 1 dim))
     )
  
  ([y m dSt noD]    ;; arity overloading
     (loop [d dSt
            rs (ef/getRiseAndSetTimes ss/SUN y m d  obs)
            tt (ef/getTransitTimes    ss/SUN y m d obs)   ]
       (println   (fymd tt)  (fhm (rs 0))  (fhm (rs 1))  (fhm tt))
       (if (= d noD)
         0
         (recur (inc d)
                (ef/getRiseAndSetTimes ss/SUN y m (inc d)  obs)
                (ef/getTransitTimes ss/SUN y m (inc d) obs)     ) )
       ) )
  )


(defn yearly "" [y fnMn] (doseq [m (range 12)] (fnMn y (inc m)))  )


(defn printES "" [y]
  (let [es  (ef/getEquinoxesAndSolsticesByYear y obs) ]
    (println (format "%4d  " (:year (es 0)) ) 
             (fmdhm (es 0))  (fmdhm (es 1))  (fmdhm (es 2))  (fmdhm (es 3)))  )
  )


;;java -cp test3.jar:t0.jar org.shetline.skyviewcafe.CmdLnRST 2018 6 20
;;2018-06-20    5:26d   azi 125.206   13:15d   alt  68.469 (decl  23.435)   azi 234.741   21:03d


(defn WGdetail "" [y m]
  (let [rs  (ef/getRiseAndSetTimes ss/SUN y m 20  obs)
        tt  (ef/getTransitTimes ss/SUN y m 20 obs)  ]
    (println (fmdhm (rs 0)) (fhm tt) (fhm (rs 1))
             (format "  %9.5f  %9.5f (%9.5f)"
                     (ss/getLongitudeDeg (ss/getHorizontalPosition
                                         ss/SUN (:val (rs 0)) obs))
                     (ss/getAltDeg (ss/getHorizontalPosition
                                   ss/SUN (:val tt) obs))
                     (ss/getLatitudeDeg (ss/getEquatorialPosition
                                         ss/SUN (:val tt) obs ss/AbrrNut))
                     ;;could add set azi (...%9.5f)
;;(ss/getLongitudeDeg (ss/getHorizontalPosition ss/SUN (:val (rs 1)) obs))
                     ))) )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

 
 (monthly 2018 6)
 (WGdetail 2018 6)
;  (yearly 2018 WGdetail)

;  (printES 2018)
  
;;2018    3-20 11:15    6-21  5:07    9-22 20:54   12-21 16:22  

;;2018	Mar 20 11:15§	Jun 21 05:07§	Sep 22 20:54§	Dec 21 16:22
  
;;Ls-MacBook-Air:toys EdV$ ./planck -m dt.equinox 2018
;;2018
;;3-20 11:14:45    6-21 5:6:42    9-22 20:53:41    12-21 16:21:52

)
