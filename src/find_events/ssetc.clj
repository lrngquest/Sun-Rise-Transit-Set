(ns find-events.ssetc
	(:require [find-events.const :as cn] ) 
	(:require [find-events.mthu  :as m] )
	(:require [find-events.vsp   :as v] )
        (:require [find-events.nuterms :as n]))



;; from o.s.a.UTConvertor
  ;; table for compatibility testing -- more current values available
  ;; from  http://stjarnhimlen.se/comp/time.html
;;Many simplifcations made
;; - recent years (i.e. after 1599) ==> span:1 in table values
;; - span:1 ==> tableMidYear : year
;; - above ==> n : 0  ==> dt2  is sole remaining term
(defn hDT "ala historicDeltaT 2000..2014" [y]
  (let [iy   (int y)
        yx   (if (> iy 2014) 0  (- iy 2000))
        ret   ( nth [63.82  64.09  64.30  64.47  64.57  64.68  64.84   65  65  66   66  66  67  69  71  ] yx)    ]
    ret) )

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
  (loop [timeJDE timeJDU   i 0]
    (if (= i 5)
      timeJDE
      (recur  (+ timeJDU (/ (getDeltaTatJulianDate timeJDE) 86400.0))
              (inc i) ) ) )
  )


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

;; from o.s.a.Ecliptic  -- orig. 0 members; class only to permit cache?

(defn NUinnerPsi "calc 1 term of psi,epsilon" [T D M M1 F Q  term]
  (let [[fD fM fM1 fF fQ cs0 cs1 cc0 cc1]  term
        arg    (+ (* D fD) (* M fM) (* M1 fM1) (* F fF) (* Q fQ))
        dpsi   (* (m/sinDeg arg)  (+ cs0 (* cs1 T)))  ]
    dpsi)
  )

(defn NUinnerEps "calc 1 term of psi,epsilon" [T D M M1 F Q  term]
  (let [[fD fM fM1 fF fQ cs0 cs1 cc0 cc1]  term
        arg    (+ (* D fD) (* M fM) (* M1 fM1) (* F fF) (* Q fQ))
        deps   (* (m/cosDeg arg)  (+ cc0 (* cc1 T)))   ]
    deps)
  )

(defn NU "calc psi,epsilon over all (63) terms" [MO T ]
  (let [T2  (* T T)      T3   (* T2 T)
        D   (+ 297.85036 (* T 445267.111480) (* T2 -0.0019142) (/ T3 189474.0))
        M   (+ 357.52772 (* T  35999.050340) (* T2 -0.0001603) (/ T3 -300000.0))
        M1  (+ 134.96298 (* T 477198.867398) (* T2  0.0086972) (/ T3 56250.0))
        F   (+  93.27191 (* T 483202.017538) (* T2  0.0036825) (/ T3 327270.0))
        Q   (+ 125.04452 (* T  -1934.136261) (* T2  0.0020708) (/ T3 450000.0))
        dpsi   (reduce + (map (partial NUinnerPsi T D M M1 F Q) n/terms) )
        deps   (reduce + (map (partial NUinnerEps T D M M1 F Q) n/terms) )
        rdpsi  (v/aAS2Rad (/ dpsi 10000.0))
        rdeps  (v/aAS2Rad (/ deps 10000.0))  ]
     ;; output full nutation based on inuut  MeanObliquity and series calc.
;    (println "NU rdpsi"rdpsi )
    [ rdpsi rdeps (+ (MO 2) rdeps)] )
  )


(def NUTATED 0)   (def MEAN_OBLIQUITY 1)

(defn getNutation "only modes: MEAN_OBLIQUITY NUTATED"  [timeJDE mode]
  (let [T      (/ (- timeJDE cn/JD_J2000) 36525.0)
        coeff  [-4680.93 -1.55 1999.25 -51.38 -249.67
                  -39.05  7.12   27.87   5.79    2.45]
        nuMO   (loop  [e 23.43929111    U (/ T 100.0)    i 0]
                 (if (= (count coeff) i)
                   [0.0 0.0  (* cn/D2Rad e)];;delta_psi delta_epsilon obliquity
                   (recur
                    (+ e (/ (* (coeff i) U ) 3600.0) )   (* U U)   (inc i) )
                   ) )  ;; always do MEAN_OBLIQUITY calc.
        out    (if (= mode NUTATED)  (NU nuMO T)  nuMO) ]
;(when (= mode NUTATED) (println " moNU fullNU"nuMO" "out))
    out)
  )


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

(defn getHeliocentricPosition "" [planet timeJDE]
  (case planet
    0    {:latitude 0  :longitude 0  :radius 0}
    2    (v/getHeliocentricPosition planet timeJDE) )
  )


(defn getEclipticPosSimple "simple case" [planet timeJDE  flags]  
  (let [earthPos  (getHeliocentricPosition EARTH  timeJDE)
        otherPos  (getHeliocentricPosition planet timeJDE)
        result    (translate2 otherPos earthPos)   ]   
    result)
  )

(defn nutateEclipticPosition3D "" [pos timeJDE] ;;assume NUTATED
  {:longitude (add_nonneg (:longitude pos) ((getNutation timeJDE NUTATED) 0))
   :latitude  (:latitude pos)
   :radius    (:radius   pos) }
  )


(def ABERRATION 128)     (def NUTATION 4)     (def AbrrNut 132)
(def L_D_Per_AU 0.005775518328)           (def LOW_PRECISION 1)

(defn getEclipticPosition "ABERRATION ==> iter" [planet timeJDE flags]
  (let [res1  (loop [i   (if (not= 0 (bit-and flags ABERRATION))  0  3)
                     pos  (getEclipticPosSimple planet timeJDE flags) ]
                (if (> i 2)
                  pos
                  (recur (inc i)
                         (getEclipticPosSimple
                          planet
                          (- timeJDE (* L_D_Per_AU (:radius pos)))
                          0 )  ) ) )
        res2  (if (not= 0 (bit-and flags NUTATION))
                (nutateEclipticPosition3D res1 timeJDE)
                res1)     ]
;(when (not= 0 (bit-and flags NUTATION)) (println "gEP res1 res2"res1" "res2 " "timeJDE))
    res2)
  )


(defn getEquatorialPosition  " "  [planet timeJDE obs flags]
  (let [eclipticPos  (getEclipticPosition planet timeJDE  flags)
        oblMode      (if (not= 0 (bit-and flags NUTATION ))
                       NUTATED   MEAN_OBLIQUITY)
        pos          (eclipticToEquatorial eclipticPos timeJDE oblMode)]
    pos)
  )


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


(defn getHorizontalPosition "" [planet timeJDU obs ]  ;; untested  TODO
  (let [flg   (bit-or ABERRATION LOW_PRECISION)  ;; ala wrapper meth.
        pos   (getEquatorialPosition planet (UT_to_TDB timeJDU) obs flg)  ]
    (equatorialToHorizontal3D pos timeJDU flg  obs)  )
  )


(def SIGNED_HOUR_ANGLE 2048) ;0x0800

(defn getHourAngle "SolarSystem" [planet timeJDU obs flags]
  (let [pos    (getEquatorialPosition planet (UT_to_TDB timeJDU) obs flags )
        gLHA   (getLocalHourAngle timeJDU false obs)
        RA     (getRightAscension pos)
        HA     (if (not= 0 (bit-and flags SIGNED_HOUR_ANGLE))
                 (subtract  gLHA RA)
                 (subtract_nonneg gLHA RA))	]
    HA)
  ) ;; limited test OK Mar20
