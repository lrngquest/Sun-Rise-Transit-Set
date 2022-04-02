(ns find-events.efx
	(:require [find-events.const :as cn] ) 
	(:require [find-events.mthu  :as m] )
	(:require [find-events.ssetc :as ss] )
        (:require [find-events.dates :as da] )
        (:require [find-events.ut2tdb :as u]) )


(def getDSTfacts
  (memoize
   (fn [yr mn da]
     (->> (da/getDSTntrvl yr) (da/getDSTfacts yr mn da )))  ) )


;; from ZeroFinder
(defn zfInner " one step of ZeroFinder" [zsf  [x1 y1 x2 y2 _ _]]
  (let [x     (- x1 (* (/ y1 (- y2 y1)) (- x2 x1)) )
        y     (zsf x)
        vxy6  (if (or (and (< y1 y2) (< y 0.0))  (and (> y1 y2) (> y 0.0)))
                [x  y   x2 y2  y x]
                [x1 y1  x  y   y x])  ]
    vxy6)  )


(defn getXAtZ "" [zeroSeekFn tolerance maxI  [x1 y1 x2 y2]]   ;; vec des.
  ;; repeatedly steps zeroFinder logic, injecting specific zeroSeekingFn
  ((reduce
    (fn [statev v]
      (if (<= (Math/abs (double (statev 4))) tolerance)
        (reduced statev)
        (zfInner zeroSeekFn statev) )   )
    (zfInner zeroSeekFn [x1 y1 x2 y2  0 0] )  (range (inc maxI)) ;; init
    ) 5)   ) ;; per original return (statev 5) : x


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
     :hour hour :minute minute :evt eventType  :val value})  )


(def vXdst
  {1380 [0 (/ 1380.0 8640) (/ 2760.0 8640) (/ 4140.0 8640)
         (/ 5520.0 8640) (/ 6900.0 8640) (/ 8280.0 8640)],
   1440 [0 (/ 1440.0 8640)  (/ 2880.0 8640) (/ 4320.0 8640)
         (/ 5760.0 8640) (/ 7200.0 8640)              1 ],
   1500 [0 (/ 1500.0 8640) (/ 3000.0 8640) (/ 4500.0 8640)
         (/ 6000.0 8640) (/ 7500.0 8640) (/ 9000.0 8640)]   }  )

(def targetAlt (+ -0.5833 -0.25) ) ;;degrees


(defn times-vals->vofv " #4 " [times values]
  (mapv vec (->> (interleave times values) (partition 4 2  )) )   )


(defn ffnRS "" [ [ _ startAlt _ endAlt ] ]
  (or (and (<= startAlt targetAlt) (<  targetAlt endAlt) )
        (and (<  endAlt targetAlt)   (<= targetAlt startAlt) ) )    )


(defn getRiseAndSetTimes "" [planet year month day obs]
  (let [tol         0.001
        maxI        8
        tzOffs      (- 0 (:TZoffs obs))
        [DSToffs minsInDay]   (getDSTfacts year month day)
        startOfDay  (da/getJD year month day 0 (- tzOffs DSToffs) -30.0)
        endOfDay    (+ startOfDay (/ minsInDay 1440.0))

        zsfRS       #(-> (ss/getHorizontalPosition planet % obs) (ss/getAltDeg )
                         (-  targetAlt)  )  ;; #1

        times       (mapv  + (repeat 7 startOfDay)  (vXdst (int minsInDay)))
        values      (mapv  #(-> (ss/getHorizontalPosition planet % obs)  ;; #1
                                (ss/getAltDeg ))  times)
        tAlp        (times-vals->vofv times values)     ;; #4   6x4  ala
    ;;  tAlp [ [t0 a0 t1 a1] [t1 a1 t2 a2] [t2 a2 t3 a3]
    ;;         [t3 a3 t4 a4] [t4 a4 t5 a5] [t5 a5 t6 a6]  ]

        fltrd-tAlp  (filter ffnRS tAlp)
        
        rise-ntrvl  (first fltrd-tAlp)
        eventTime   (getXAtZ zsfRS tol maxI  rise-ntrvl)
        riseTime    (* 24.0 (- eventTime startOfDay))
        rt (createEventAtHourOffset
            year month day minsInDay riseTime "rise"   eventTime obs)

        set-ntrvl   (last fltrd-tAlp)
        eventTime2  (getXAtZ zsfRS tol maxI  set-ntrvl)
        setTime     (* 24.0 (- eventTime2 startOfDay))
        st (createEventAtHourOffset
            year month day minsInDay setTime "set" eventTime2 obs)   ]
    [rt st]  )   )


(defn getTransitTimes "" [planet year month day obs]
  (let [tzOffs          (- 0 (:TZoffs obs))
        [DSToffs minsInDay]     (getDSTfacts year month day)
        startOfDay      (da/getJD year month day 0 (- tzOffs DSToffs) -30.0)
        minutesInDay    1440.0 ;;use minsInDay?? TODO
                         ;; #2
        getHourAngl     #(ss/getHourAngle planet obs ss/SIGNED_HOUR_ANGLE %)
 
        times           (mapv + (repeat 6 startOfDay) '(0 0.2 0.4 0.6 0.8 1.0))
        values          (mapv getHourAngl times) 
        tHap            (times-vals->vofv times values)  ;; #4   5x4

        ;; filter checking e.g   [ _ ha0 _ ha1] ... wrt 0.0    ;; #3
        trns-ntrvl      (first (filter #(and(<= (% 1) 0.0) (> (% 3) 0.0)) tHap))
        eventTime       (getXAtZ getHourAngl 0.0001 8  trns-ntrvl)
        
        ;;ASSERT (>= (getHorizontalPosition ...)  minAltRad) ==>always accepted

        transTime    (* 24.0 (- eventTime startOfDay))   ]
    (createEventAtHourOffset
     year month day minutesInDay transTime "transit"   eventTime obs)  )   )


(defn getEquinoxSolsticeEvent "ASSERT m in (3,6,9,12)" [y m d obs]
  (let [[DSToffs minsInDay]   (getDSTfacts y m d)
        
        tzOffs     (- 0 (:TZoffs obs))
        startDay   (da/getJD y m d 0 (- tzOffs DSToffs) -30.0)
        startOfDay (u/UT_to_TDB startDay)
        endOfDay   (u/UT_to_TDB (+ startDay (/ minsInDay 1440.0)))

        loLong0    (ss/getLongitudeDeg
                    (ss/getEclipticPosition ss/SUN startOfDay ss/AbrrNut) )
        hiLong0    (ss/getLongitudeDeg
                    (ss/getEclipticPosition ss/SUN   endOfDay ss/AbrrNut) )
        lowLong    (if (> loLong0 315.0)  (- loLong0 360.0)   loLong0)
        highLong   (if (> hiLong0 315.0)  (- hiLong0 360.0)   hiLong0)    
        angle      (- (* m 30) 90.0)    ;; m:3,6,9,12 ==> 0,90,180,270 resp.
        xyv        [ startOfDay (- lowLong angle) endOfDay (- highLong angle) ]

        val     (getXAtZ #(->(ss/getEclipticPosition ss/SUN % ss/AbrrNut)
                             (ss/getLongitudeDeg  ) (-  angle) (m/rmod2  360.0))
                          0.00001 6  xyv )  ;; tol maxI    ;; #1
        
        eventTm    (* 24.0 (- val startOfDay) )
        es         (if (odd? m) "equinox"  "solstice" )    ]
    (if  (and (<= lowLong angle) (< angle highLong))
      (createEventAtHourOffset y m d minsInDay eventTm es 0 obs)  "") )  )


(defn iterESdays [obs year month]
  (reduce  (fn [acc day] ;; accum current-item
             (let [e  (getEquinoxSolsticeEvent year month day obs) ]
               (if (not= "" e) (reduced e)  acc) ) )
           []     [19 20 21 22 23]  )  )   ;;init-val  coll to op. on

(defn getEquinoxesAndSolsticesByYear "" [year obs]
  (mapv (partial iterESdays obs year) '(3 6 9 12) )  )



;; 2021 Dec  changes
;;
;; #1  Replaced the use of a param-reordering-fn and a use of (partial...)
;;  with a single function literal (May relyi on references to bound
;;  ids in a surrounding (let...) ) to provide the required
;;  single-param fn.  See    getRiseAndSetTimes  getEquinoxSolsticeEvent.

;; #2  Replaced a use of (partial...) with a function literal (again relying
;;  on bound refs) to provide the single-param fn needed.

;; #3  Replaced a simple filter-fn with a function literal.

;; #4  Replaced hard-wired formation of start-end combinations of #time,value
;;  with a new fn using core fns  interleave, partition
;;  (hat tip to Andrey Listopadov 2021-12-1 Advent-of-Code Day 1).

;; Replaced all uses of  (vec (map...))  with  (mapv...)
;; Replaced 3 (def...) and use in (case...) with a table keyed-by-int
;;  and simple map-reference.


;; 2020 Feb  replaced (loop...) with (reduce...)
