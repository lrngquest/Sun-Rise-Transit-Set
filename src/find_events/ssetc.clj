(ns find-events.ssetc
	(:require [find-events.const :as cn] ) 
	(:require [find-events.mthu  :as m] )
	(:require [find-events.vsp   :as v] )
        (:require [find-events.nuterms :as n])
        (:require [find-events.ut2tdb :as u]) )



;; from o.s.m.Angle  -- orig. 1 member
(defn atan2_nonneg ""    [ y x ]   (m/rmod (Math/atan2 y x) cn/TwoPI))
(defn add_nonneg ""      [L1 L2]   (m/rmod (+ L1 L2) cn/TwoPI))
(defn subtract_nonneg "" [L1 L2]   (m/rmod (- L1 L2) cn/TwoPI))
(defn subtract ""        [L1 L2]   (m/rmod2 (- L1 L2) cn/TwoPI))
(defn angleCtor ""       [L] (m/rmod2 L cn/TwoPI) ) ;; RANGE_LIMIT_SIGNED


;; from o.s.m.SphericalPosition3D
(defn convertRectangular "" [x y z]
  {:longitude (atan2_nonneg y x)
   :latitude  (Math/atan2 z (Math/sqrt (+(* x x) (* y y))))
   :radius    (Math/sqrt (+ (* x x) (* y y) (* z z))) } )  ;; SP3D as map


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
   
(defn getLongitudeDeg ""      [pos]  (* cn/R2Deg (:longitude pos)) )
(defn getLatitudeDeg  ""      [pos]  (* cn/R2Deg (:latitude  pos)) )
(defn getRightAscension ""    [pos]  (:longitude pos))
(defn getDeclination  ""      [pos]  (:latitude  pos))
(defn getRightAscensionDeg "" [pos]  (getLongitudeDeg pos) )
(defn getDeclinationDeg ""    [pos]  (getLatitudeDeg  pos) )
(defn getAltDeg ""            [pos]  (* cn/R2Deg (:latitude  pos)) )

;; from o.s.a.Ecliptic

(defn NU "calc psi,epsilon over all (63) terms" [MO T ]
  (let [T2  (* T T)      T3   (* T2 T)
        D   (+ 297.85036 (* T 445267.111480) (* T2 -0.0019142) (/ T3 189474.0))
        M   (+ 357.52772 (* T  35999.050340) (* T2 -0.0001603) (/ T3 -300000.0))
        M1  (+ 134.96298 (* T 477198.867398) (* T2  0.0086972) (/ T3 56250.0))
        F   (+  93.27191 (* T 483202.017538) (* T2  0.0036825) (/ T3 327270.0))
        Q   (+ 125.04452 (* T  -1934.136261) (* T2  0.0020708) (/ T3 450000.0))
 
   ;; use destruc on param exposing contents af a single "term"
   ;; access  T D M M1 F Q  as bound in (current) let
   ;; Thus evaluate each term for psi,eps respectively, and sum.
        [dpsi deps]
        (reduce
             (fn [accumv  [fD fM fM1 fF fQ cs0 cs1 cc0 cc1]]
               (let [arg  (+ (* D fD) (* M fM) (* M1 fM1) (* F fF) (* Q fQ)) ]
                 (mapv + accumv
                       [ (* (m/sinDeg arg)  (+ cs0 (* cs1 T)))
                         (* (m/cosDeg arg)  (+ cc0 (* cc1 T))) ] )  )   )
             [0.0 0.0]  n/terms ) ;;init
        
        rdpsi  (v/aAS2Rad (/ dpsi 10000.0))
        rdeps  (v/aAS2Rad (/ deps 10000.0))    ]
 ;; full nutation based on input  MeanObliquity and series calc.
    [ rdpsi rdeps (+ (MO 2) rdeps)] )  )


(defn getNutationMO  " MEAN_OBLIQUITY only "  [T ]
  (let [coeff  [-4680.93 -1.55 1999.25 -51.38 -249.67
                  -39.05  7.12   27.87   5.79    2.45]
        [e U]  (reduce  (fn [[ae aU] v]
                          [ (+ ae (/ (* v aU ) 3600.0) )   (* aU aU) ]   )
                        [ 23.43929111  (/ T 100.0) ]    coeff)             ]
    [0.0  0.0  (* cn/D2Rad e)]  )   )


(defn getNutation ""  [timeJDE mode]
  (let [T      (/ (- timeJDE cn/JD_J2000) 36525.0) ]
    (case mode
      2    [0.0  0.0  (* cn/D2Rad 23.43929111) ]      ;; 2: J2000
      1    (getNutationMO T)                          ;; 1: MEAN_OBLIQUITY
      0    (->  (getNutationMO T)  (NU  T)) )  )   )  ;; 0: NUTATED




(def NUTATED 0)   (def MEAN_OBLIQUITY 1)

(defn eclipticToEquatorial "" [pos timeJDE mode]
  (let [nutation  (getNutation timeJDE mode)
        L         (:longitude pos)  ;; aka RightAscension
        B         (:latitude  pos)  ;; aka Declination
        E         (nutation 2)  ]   ;; 2::OBLIQUITY
    {:longitude (atan2_nonneg (- (* (Math/sin L) (Math/cos E))
                                 (* (Math/tan B) (Math/sin E)))
                            (Math/cos L) )
     :latitude  (Math/asin
                 (m/limitNegOneToOne (+ (* (Math/sin B) (Math/cos E))
                                      (* (Math/cos B) (Math/sin E) (Math/sin L))
                                      ) ) )
     :radius (:radius pos) }  )  ;; thus a SP3D
  ) ;; init test OK Sep06


;; from o.s.a.SolarSystem
(defn getGreenwichMeanSiderealTime  ""  [ timeJDU]
  (let [t (- timeJDU cn/JD_J2000)   T (/ t 36525)   T2 (* T T)   T3 (* T2 T)]
    (m/rmod
     (- (+ 280.46061837 (* 360.98564736629 t) (* 0.000387933 T2))
        (/ T3 38710000.0))
     360.0)  )
  )

(def SUN 0)   (def EARTH 2)

(defn getHeliocentricPosition "" [^long planet timeJDE]
  (case planet
    0    {:latitude 0  :longitude 0  :radius 0}
    2    (v/getHeliocentricPosition planet timeJDE) )  )


(defn getEclipticPosSimple "simple case" [planet timeJDE  ]  
  (translate2 (getHeliocentricPosition planet timeJDE)
              (getHeliocentricPosition EARTH  timeJDE) )  )


(defn nutateEclipticPosition3D "" [pos timeJDE] ;;assume NUTATED
  {:longitude (add_nonneg (:longitude pos) ((getNutation timeJDE NUTATED) 0))
   :latitude  (:latitude pos)
   :radius    (:radius   pos) }  )


(def ABERRATION 128)     (def NUTATION 4)     (def AbrrNut 132)
(def L_D_Per_AU 0.005775518328)           (def LOW_PRECISION 1)


(defn getEclipticPosition  "ABERRATION ==> iter"  [planet timeJDE flags]
  (let [zrng  (if (not= 0 (bit-and flags ABERRATION))   (range 2)   '() )
        
        res1  (reduce
                 (fn [a-pos v]
                   (getEclipticPosSimple
                          planet (- timeJDE (* L_D_Per_AU (:radius a-pos))) )  )
                 (getEclipticPosSimple planet timeJDE ) ;; pos
                 zrng)            ] ;; '() ==> no iterations, use 'pos'

    (if (not= 0 (bit-and flags NUTATION))
                (nutateEclipticPosition3D res1 timeJDE)
                res1) )    )


(defn getEquatorialPosition  " "  [planet timeJDE obs flags]
  (let [eclipticPos  (getEclipticPosition planet timeJDE  flags)
        oblMode      (if (not= 0 (bit-and flags NUTATION ))
                       NUTATED   MEAN_OBLIQUITY)
        pos          (eclipticToEquatorial eclipticPos timeJDE oblMode)]
    pos)  )


(defn getLocalHourAngle "SkyObserver" [timeJDU apparent obs]
  (let [gst  (getGreenwichMeanSiderealTime timeJDU) ;;case apparent::false !
        ngl  (add_nonneg (angleCtor (* cn/D2Rad gst))  (:longitude obs))  ]
    ngl) ;;radians   ;; original had caching FWIW
  ) ;; limited test OK Mar20


(defn equatorialToHorizontal3D "skyobserver p.7" [pos timeJDU flags  obs]
  (let [lat   (:latitude obs)      ;;others?
        lha   (getLocalHourAngle timeJDU false obs) ;;f==>no NUTATION s/b ok
        RA    (getRightAscension pos)
        d     (getDeclination    pos)
        H     (- lha RA)
        azi   (Math/atan2 (Math/sin H) (- (* (Math/cos H) (Math/sin lat))
                                          (* (Math/tan d) (Math/cos lat)) ) )
        alt   (Math/asin (m/limitNegOneToOne
                          (+ (* (Math/sin lat) (Math/sin d) )
                             (* (Math/cos lat) (Math/cos d) (Math/cos H)))) ) ]
    {:longitude azi   :latitude alt   :radius (:radius pos) }   )
  )  ;;limited test OK Mar21


(defn getHorizontalPosition "arity overload "
   ([planet timeJDU obs ]
      (getHorizontalPosition planet timeJDU obs ;; v-- wrapper in orig java
                             (bit-or ABERRATION LOW_PRECISION)) )
   ([planet timeJDU obs flg ]
      (-> (getEquatorialPosition planet (u/UT_to_TDB timeJDU) obs flg)
          (equatorialToHorizontal3D   timeJDU flg  obs)) )   )


(def SIGNED_HOUR_ANGLE 2048) ;0x0800

(defn getHourAngle "SolarSystem" [planet obs flags  timeJDU]
  (let [pos    (getEquatorialPosition planet (u/UT_to_TDB timeJDU) obs flags )
        gLHA   (getLocalHourAngle timeJDU false obs)
        RA     (getRightAscension pos)
        HA     (if (not= 0 (bit-and flags SIGNED_HOUR_ANGLE))
                 (subtract  gLHA RA)
                 (subtract_nonneg gLHA RA))	]
    HA)  )
