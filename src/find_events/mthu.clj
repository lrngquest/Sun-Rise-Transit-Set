(ns find-events.mthu )

;; MathUtil
(defn ifloor "" [x]     (int (Math/floor x)) )
(defn limitNegOneToOne "" [x]
  (cond (< x (- -1 0.01))  -1.0  ;; tolerance::0.01
        (> x (+  1 0.01))   1.0
        (< x -1.0)         -1.0
        (> x  1.0)          1.0
        :else               x )  )
(defn rmod   "" [ x y]  (- x (* (Math/floor (/ x y)) y) )  )
(defn rmod2  "" [ x y]
  (let [result1  (- x (* y (Math/floor (/ x y)) ))
        result2  (if (>= result1 (/ y 2.0))   (- result1 y)   result1)  ]
    result2) )

(defn sinDeg ""  [deg] (Math/sin (* deg (/ Math/PI 180.0))) )
(defn cosDeg ""  [deg] (Math/cos (* deg (/ Math/PI 180.0))) )
