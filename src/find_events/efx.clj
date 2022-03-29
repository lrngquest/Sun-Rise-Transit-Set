(ns find-events.efx
	(:require [find-events.const :as cn] ) 
	(:require [find-events.mthu  :as m] )
	(:require [find-events.ssetc :as ss] )
        (:require [find-events.dates :as da] ) )


(defn getDSTfacts "" [year month day]  ;;refactor to avoid multi calls?
  (->> (da/getDSTntrvl year) (da/getDSTfacts year month day ))  )

;; from ZeroFinder

(defn zfInner "logic for one step of ZeroFinder" [zsf newv]
  (let [[x1 y1 x2 y2 u1 u2]  newv;; vec destructure
        x     (- x1 (* (/ y1 (- y2 y1)) (- x2 x1)) )
        y     (zsf x)
        vxy6  (if (or (and (< y1 y2) (< y 0.0))  (and (> y1 y2) (> y 0.0)))
                [x  y   x2 y2  y x]
                [x1 y1  x  y   y x])  ]
    vxy6)
  )


(defn getXAtZ "" [zeroSeekFn tolerance maxI  xy]
  ;; repeatedly steps zeroFinder logic, injecting specific zeroSeekingFn

  (let [[x1 y1 x2 y2]  xy]
    (loop [iterCount  1
           statev     (zfInner zeroSeekFn [x1 y1 x2 y2  0 0] ) ]
 ;;    (println "iterCount "iterCount " y "(statev 4) )
     (if (or (<= (Math/abs (statev 4)) tolerance)  (>= iterCount maxI))
       (statev 5) ;; thus return x
       (recur     (inc iterCount)   (zfInner zeroSeekFn statev)) ) )  )
  )



;;from EventFinder
(defn createEventAtHourOffset ""
  [year month day minsInDay hrOffs0 eventType value obs]
  (let [hrsInDay   (/ minsInDay 60.0)
        hrOffset   (cond  (< hrOffs0 0.0)        0.0
                          (>= hrOffs0 hrsInDay)  (- hrsInDay  0.01)
                          :else                  hrOffs0    )
        hour       (m/ifloor hrOffset)
        minute     (m/ifloor (* (- hrOffset hour) 60.0))  ]
    {:year year :month month :day day
     :hour hour :minute minute :evt eventType  :val value})
  )


(def v1380 [0 (/ 1380.0 8640) (/ 2760.0 8640) (/ 4140.0 8640)
            (/ 5520.0 8640) (/ 6900.0 8640) (/ 8280.0 8640)] )
(def v1440 [0 (/ 1440.0 8640)  (/ 2880.0 8640) (/ 4320.0 8640)
            (/ 5760.0 8640) (/ 7200.0 8640)              1 ] )
(def v1500 [0 (/ 1500.0 8640) (/ 3000.0 8640) (/ 4500.0 8640)
            (/ 6000.0 8640) (/ 7500.0 8640) (/ 9000.0 8640)] )

(defn gHP "reorder params to for (partial...)" [planet obs timeJDU]
  (ss/getAltDeg (ss/getHorizontalPosition planet timeJDU obs)) )


(def targetAlt (+ -0.5833 -0.25) ) ;;degrees

(defn gaHP "for (partial...), for zero-seek-fn" [targetAlt planet obs timeJDU]
  (- (ss/getAltDeg (ss/getHorizontalPosition planet timeJDU obs)) targetAlt) )


(defn ffnRS "" [x1y1x2y2]
  (let [[u1 startAlt u2 endAlt ] x1y1x2y2]
    (or (and (<= startAlt targetAlt) (<  targetAlt endAlt) )
        (and (<  endAlt targetAlt)   (<= targetAlt startAlt) ) ))    )


(defn getRiseAndSetTimes "" [planet year month day obs]
  (let [tol         0.001
        maxI        8
        tzOffs      (- 0 (:TZoffs obs))
        [DSToffs minsInDay]   (getDSTfacts year month day)
        startOfDay  (da/getJD year month day 0 (- tzOffs DSToffs) -30.0)
        endOfDay    (+ startOfDay (/ minsInDay 1440.0))

        gHPX        (partial gHP planet obs)  ;;time only remaning param
        zsfRS       (partial gaHP targetAlt planet obs) ;; x

        dfl    (case minsInDay 1380 v1380  1440 v1440  1500 v1500 )

        times           (vec (map + (repeat 7 startOfDay)  dfl))
        [t0 t1 t2 t3 t4 t5 t6]     times
        [a0 a1 a2 a3 a4 a5 a6]   (vec (map gHPX times))

        tAlp [ [t0 a0 t1 a1] [t1 a1 t2 a2] [t2 a2 t3 a3]
               [t3 a3 t4 a4] [t4 a4 t5 a5] [t5 a5 t6 a6] ]

        sTsAeTeA    (first (filter ffnRS tAlp))
        eventTime   (getXAtZ zsfRS tol maxI  sTsAeTeA)
        riseTime    (* 24.0 (- eventTime startOfDay))
        rt (createEventAtHourOffset year month day minsInDay riseTime "rise"   eventTime obs)

        sTsAeTeA2   (last (filter ffnRS tAlp))
        eventTime2  (getXAtZ zsfRS tol maxI  sTsAeTeA2)
        setTime     (* 24.0 (- eventTime2 startOfDay))
        st (createEventAtHourOffset year month day minsInDay setTime "set" eventTime2 obs)   ]
    [rt st]  )
  )


(defn gAHw "wrap and reorder params" [planet obs flags timeJDU]
  (ss/getHourAngle planet timeJDU obs flags) )

(defn ffn "TT filter" [quad] (and (<= (quad 1) 0.0) (> (quad 3) 0.0) ) )


(defn getTransitTimes "" [planet year month day obs]
  (let [tzOffs          (- 0 (:TZoffs obs))
        [DSToffs minsInDay]   (getDSTfacts year month day)
        startOfDay      (da/getJD year month day 0 (- tzOffs DSToffs) -30.0)

        minutesInDay    1440.0 ;;use minsInDay?? TODO

        getHourAngleX   (partial  gAHw planet obs ss/SIGNED_HOUR_ANGLE)
        
        times           (vec (map + (repeat 6 startOfDay)
                                  '(0 0.2 0.4 0.6 0.8 1.0)))
        [t0 t1 t2 t3 t4 t5]         times
        [ha0 ha1 ha2 ha3 ha4 ha5]   (vec (map getHourAngleX times)) 

        tHap [ [t0 ha0 t1 ha1]  [t1 ha1 t2 ha2]  [t2 ha2 t3 ha3]
               [t3 ha3 t4 ha4]  [t4 ha4 t5 ha5]  ]

        sTsAeTeA        (first (filter ffn tHap))
        eventTime       (getXAtZ getHourAngleX 0.0001 8  sTsAeTeA)
        
        ;;ASSERT (>= (getHorizontalPosition ...)  minAltRad) ==>always accepted

        transTime    (* 24.0 (- eventTime startOfDay))   ]
    (createEventAtHourOffset year month day minutesInDay transTime "transit"   eventTime obs)
    )
  )


(defn gEPx "zeroSeekFn eqninox,solstice" [planet angle timeJDE]
  (m/rmod2 (- (ss/getLongitudeDeg
               (ss/getEclipticPosition planet timeJDE ss/AbrrNut))
              angle)
         360.0) )


(defn getEquinoxSolsticeEvent "ASSERT m in (3,6,9,12)" [y m d obs]
  (let [tol        0.00001
        maxI       6

        [DSToffs minsInDay]   (getDSTfacts y m d)
        tzOffs     (- 0 (:TZoffs obs))
        startDay   (da/getJD y m d 0 (- tzOffs DSToffs) -30.0)
        startOfDay (ss/UT_to_TDB startDay)
        endOfDay   (ss/UT_to_TDB (+ startDay (/ minsInDay 1440.0)))

        loLong0    (ss/getLongitudeDeg
                    (ss/getEclipticPosition ss/SUN startOfDay ss/AbrrNut) )
        hiLong0    (ss/getLongitudeDeg
                    (ss/getEclipticPosition ss/SUN   endOfDay ss/AbrrNut) )
        lowLong    (if (> loLong0 315.0)  (- loLong0 360.0)   loLong0)
        highLong   (if (> hiLong0 315.0)  (- hiLong0 360.0)   hiLong0)    
        angle      (- (* m 30) 90.0)    ;; m:3,6,9,12 ==> 0,90,180,270 resp.
        xyv        [ startOfDay (- lowLong angle) endOfDay (- highLong angle) ]
        val        (getXAtZ (partial gEPx ss/SUN angle)  tol maxI  xyv)
        eventTm    (* 24.0 (- val startOfDay) )
        es         (if (odd? m) "equinox"  "solstice" )    ]
    (if  (and (<= lowLong angle) (< angle highLong))
      (createEventAtHourOffset y m d minsInDay eventTm es 0 obs)   "") )
  )

(defn iterESdays "" [obs year month]
  (loop [day 19    event (getEquinoxSolsticeEvent year month day obs) ]
    (if (or (not= "" event) (> day 23))
      event
      (recur  (inc day)  (getEquinoxSolsticeEvent year month day obs) ) )  )
  )

(defn getEquinoxesAndSolsticesByYear "" [year obs]
  (vec (map (partial iterESdays obs year) '(3 6 9 12) ))  )
