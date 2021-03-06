(ns find-events.vsp
  (:require [find-events.const   :as cn]  [find-events.mthu    :as m]  )
;;  (:require [find-events.vsconst :as vc] )
  (:require [clojure.java.io  :as cj] [clojure.edn  :as edn])    )


;; Reading edn works with  bb  as well if classpath is extended !
;;   $ bb -cp src:resources  -m find-events.core

(def vsopc (edn/read-string (slurp (cj/resource "data.clj")) )  )


(defn vinner "eval a single term" [t [A B C]]
    (* A (Math/cos (+ B (* C t))))  )


(defn seriesCalc "" [ t seriesVsConsts]
  (reduce + (map (partial vinner t) seriesVsConsts ) )   )

(defn aAS2Rad "" [a] (* (/ a 648000.0) Math/PI) )
(defn atan2_nonneg "" [ y x]  (m/rmod (Math/atan2 y x) cn/TwoPI ))


;; from o.s.m.SphericalPosition3D
(defn convertVSOPToF5K "" [pos timeJDE]
  (let [L   (:longitude pos)      B   (:latitude  pos)        
        T   (/ (- timeJDE cn/JD_J2000) 36525.0)
        L1  (* cn/D2Rad (+ (* cn/R2Deg L) (* -1.397 T) (* -0.00031 T T)))
        dL  (aAS2Rad (+ -0.09033
                        (* 0.03916  (+ (Math/cos L1) (Math/sin L1))
                        			(Math/tan B)) )  )        
        dB  (aAS2Rad (* 0.03916  (- (Math/cos L1)  (Math/sin L1)) ))        ]
    ;(println "L "L " B "B)
    ;(println "T "T " L1 "L1 ) (println "dL "dL " dB "dB)
    { :longitude (m/rmod (+ L dL) cn/TwoPI) ;;inline fn addNonNeg 
     :latitude (+ B dB)  :radius (:radius pos)} )
  )


(defn getHeliocentricPosition "" [p timeJDE] ;; p :: 'planet' 
  (let [t  (/ (- timeJDE cn/JD_J2000) 365250.0)
        t2 (* t t)   t3 (* t2 t)   t4 (* t2 t2)   t5 (* t3 t2)
        Tcol  [1.0 t t2 t3 t4 t5]
        
        X  (reduce + (map * Tcol (map (partial seriesCalc t) ((vsopc p) 0) )))
        Y  (reduce + (map * Tcol (map (partial seriesCalc t) ((vsopc p) 1) )))
        Z  (reduce + (map * Tcol (map (partial seriesCalc t) ((vsopc p) 2) )))
        tmppos {:longitude (m/rmod X cn/TwoPI)  :latitude Y  :radius Z} ;SP3D ctor
        pos   (convertVSOPToF5K tmppos timeJDE)        ]
   ; (println "x"X " y"Y " z"Z)
   ; (println "tmppos "tmppos ) (println "pos "pos)
    pos)
  )



;;  uncomment   (:require [find-events.vsconst :as vc] )  above and :
;;(defn -main "" []  (println "= vsconst " (= vsopc vc/vsconst) ) )


