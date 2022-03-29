(ns find-events.vsp
	(:require [find-events.vsconst :as vcn] )
	(:require [find-events.const   :as cn] )
	(:require [find-events.mthu    :as m] ))

(defn vinner "eval a single term" [t ABC]
  (let [[A B C] ABC
        term    (* A (Math/cos (+ B (* C t))))  ]
    ;(println "A "A " B "B " C "C " term "term)
    term )
  )


(defn seriesCalc "" [ t seriesVsConsts]
  (reduce + (map (partial vinner t) seriesVsConsts ) )   )

(defn aAS2Rad "" [a] (* (/ a 648000.0) Math/PI) )
(defn atan2_nonneg "" [ y x]  (m/rmod (Math/atan2 y x) cn/TwoPI ))


;; from o.s.m.SphericalPosition3D
(defn convertRectangular "" [x y z]
  {:longitude (atan2_nonneg y x)
   :latitude  (Math/atan2 z (Math/sqrt (+(* x x) (* y y))))
   :radius    (Math/sqrt (+ (* x x) (* y y) (* z z))) } )  ;; SP3D as map


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
        
        X  (reduce + (map * Tcol (map (partial seriesCalc t) ((vcn/vsconst p) 0) )))
        Y  (reduce + (map * Tcol (map (partial seriesCalc t) ((vcn/vsconst p) 1) )))
        Z  (reduce + (map * Tcol (map (partial seriesCalc t) ((vcn/vsconst p) 2) )))
        tmppos {:longitude (m/rmod X cn/TwoPI)  :latitude Y  :radius Z} ;SP3D ctor
        pos   (convertVSOPToF5K tmppos timeJDE)        ]
   ; (println "x"X " y"Y " z"Z)
   ; (println "tmppos "tmppos ) (println "pos "pos)
    pos)
  )


;; ala o.s.a.SolarSystem  to allow testing  getEclipticPosition
(defn translate2 "" [membPos newOrig]
  (let [L0 (:longitude newOrig)  B0 (:latitude newOrig)  R0 (:radius newOrig)
        L  (:longitude membPos)  B  (:latitude membPos)  R  (:radius membPos)
        x  (- (* R  (Math/cos B ) (Math/cos L ))
              (* R0 (Math/cos B0) (Math/cos L0)) )
        y  (- (* R  (Math/cos B ) (Math/sin L ))
              (* R0 (Math/cos B0) (Math/sin L0)) )
        z  (- (* R  (Math/sin B)) (* R0 (Math/sin B0)) )   ]
    (convertRectangular x y z) )
)


(defn getEclipticPosition "simple case" [planet timeJDE ]  
  (let [;;earthPos  (getHeliocentricPosition EARTH earthTime) ;3 param ver.
        earthPos  (getHeliocentricPosition 2      timeJDE)  ;EARTH :: 2
        otherPos  (getHeliocentricPosition planet timeJDE)
        result    (translate2 otherPos earthPos)   ]   
;(println "earthPos "earthPos) (println "otherPos "otherPos) (println result)
    result)
  )


(def LIGHT_DAYS_PER_AU 0.005775518328) ;AstroConstants

(defn lightDelayT2 "" [planet timeJDE]
  (loop [distance (:radius (getEclipticPosition planet timeJDE))   i 0]
    (if (> i 2)
      (- timeJDE (* LIGHT_DAYS_PER_AU distance)) ;"return"
      (recur (:radius
              (getEclipticPosition planet
                                   (- timeJDE (* LIGHT_DAYS_PER_AU distance)) ))
             (inc i) )  )  )
  );; test OK Dec20

(defn calcLightDelay "" [planet timeJDE]
  (- timeJDE (lightDelayT2 planet timeJDE)) )


