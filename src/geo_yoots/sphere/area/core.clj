(ns geo-yoots.sphere.area.core
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))



;; -----------
;; - IMPL 1
;; -----------



;; ===========
;; - IMPL 2
;; -----------
;; Algorithm
;; ---
;; Transform polgyon projection plane into XY and apply shoelace algorithm
;;
;; Plane rotation algo:
;;   https://math.stackexchange.com/questions/1435018/change-a-3d-plane-to-xy-plane

(defn ** [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

(defn sq
  [x]
  (** x 2))


;; ----
;; - Helpers
;; ----

(defn xx+yy
  [x y]
  (+ (sq x) (sq y)))

(defn sqrt-xx+yy+zz
  [x y z]
  (Math/sqrt (+ (sq x) (sq y) (sq z))))

(defn a-div-xx+yy
  [a x y]
  (/ a (xx+yy x y)))

(defn a-div-sqrt-xx+yy+zz
  [a x y z]
  (/ a (sqrt-xx+yy+zz x y z)))



;; ---
;; - Columns
;; ---

(def X 0)
(def Y 1)
(def Z 2)

(defn _00
  [x y z]
  (+
    (a-div-xx+yy (sq y) x y)
    (a-div-sqrt-xx+yy+zz (* (- 1 (a-div-xx+yy (sq y) x y)) z) x y z)))

(defn _01
  [x y z]
  (a-div-xx+yy (* -1 x y (- 1 (a-div-sqrt-xx+yy+zz z x y z))) x y))

(defn _02
  [x y z]
  (* -1 (a-div-sqrt-xx+yy+zz x x y z)))

(defn _10
  [x y z]
  (_01 x y z))

(defn _11
  [x y z]
  (+
    (a-div-xx+yy (sq x) x y)
    (a-div-sqrt-xx+yy+zz (* (- 1 (a-div-xx+yy (sq x) x y)) z) x y z)))

(defn _12
  [x y z]
  (* -1 (a-div-sqrt-xx+yy+zz y x y z)))

(defn _20
  [x y z]
  (a-div-sqrt-xx+yy+zz x x y z))

(defn _21
  [x y z]
  (a-div-sqrt-xx+yy+zz y x y z))

(defn _22
  [x y z]
  (a-div-sqrt-xx+yy+zz z x y z))


(defn rotation-matrix-2
  [norm]
  (let [x (.get norm X)
        y (.get norm Y)
        z (.get norm Z)]
    (mtx/matrix [[(_00 x y z) (_01 x y z) (_02 x y z)]
                 [(_10 x y z) (_11 x y z) (_12 x y z)]
                 [(_20 x y z) (_21 x y z) (_22 x y z)]])))


;; -----------
;; - Area Function
;; ----------


;; https://en.wikipedia.org/wiki/Shoelace_formula

;; rot-mtx[3 x 3] X pt-mtx[3 x 1] => 3 x 1
(defn rotate
  [rot-mtx pt]
  (mtx/inner-product rot-mtx pt))

(defn shoelace-matrix
  [vertices]
  (let [pvtx        (geo.sphere.util/vertices->projection-plane vertices)
        plane-norm  (geo.sphere.util/polygon->normal pvtx)]  ;; normal vector or original plane
    #_(println (format "PROJECTED_VERTICES=%s" pvtx))
    (let [rot-mtx   (rotation-matrix-2 plane-norm)]
      #_(println (format "ROTATION_MATRIX=%s" rot-mtx))
      (loop [xs pvtx
             acc []]
        (if-let [x (first xs)]
          (let [vtx (mtx.op/* geo.const/earth-radius (rotate rot-mtx x))]
            #_(println (format "TRANSLATING POINT= (%s -> %s" x vtx))
            (recur (rest xs) (conj acc [(.get vtx X) (.get vtx Y)])))
          (mtx/matrix acc))))))

(defn apply-shoelace
  [pts]
  (let [n (.rowCount pts)]
    (loop [i  0
           acc 0]
      (if (< i n)
        (let [i_idx   (mod i n)
              i+1_idx (mod (inc i) n)
              x_i   (.get pts i_idx   X)
              y_i+1 (.get pts i+1_idx Y)
              x_i+1 (.get pts i+1_idx X)
              y_i   (.get pts i_idx   Y)]
          #_(println (format "%s * %s - %s * %s" x_i y_i+1 x_i+1 y_i))
          (recur (inc i)
                 ( + acc (- (* x_i y_i+1) (* x_i+1 y_i)))))
        (/ (Math/abs acc) 2)))))

(defn polygon
  [vertices]
  (apply-shoelace (shoelace-matrix vertices)))



;; TODO: Turn into formal unit tests
;; ===========
;; -  Tests  -
;; ==========

;; 24740.08
(def poly1 [[ 1.000000  0.000000]
            [ 0.000000  1.000000]
            [-1.000000  0.000000]
            [ 0.000000 -1.000000]])

;; 338550.27
(def poly2 [[-15.290669  179.159774]
            [-11.469978  179.626362]
            [ -9.559677 -177.255858]
            [-13.678565 -174.857297]
            [-17.228967 -177.598549]])

;; 1.93
(def poly3 [[29.99906170493383 -90.42725561653577]
            [29.99533640496636 -90.42696669066818]
            [29.99335867823883 -90.41534213633119]
            [29.99018024847489 -90.40729174699797]
            [29.98605085649548 -90.40184120993824]
            [29.98085079882353 -90.39800724559494]
            [29.97928333955896 -90.39690344594437]
            [29.98075305584882 -90.39218453030135]
            [29.98740135552484 -90.39608179446184]
            [29.99207957444455 -90.39965704606774]
            [29.99557763234701 -90.40881580618841]
            [29.99800787798624 -90.41721422776384]
            [29.99906170493383 -90.42725561653577]])

;; 1.49
(def poly4 [[29.993094999999997 -90.460828]
            [29.992001000000002 -90.454835]
            [29.990195999999997 -90.446007]
            [29.99019           -90.43206]
            [29.994350999999995 -90.432192]
            [29.994384999999994 -90.434352]
            [29.995379          -90.445602]
            [29.996012000000007 -90.451396]
            [29.997289999999992 -90.45873]
            [29.997392000000005 -90.458726]
            [29.997392000000005 -90.45888]
            [29.997426000000004 -90.459535]
            [29.993094999999997 -90.460828]])

;; 6.146
(def poly5 [[52.971092          4.7655129999999986]
            [52.95706000000001  4.764809000000014]
            [52.95607699999999  4.769634999999994]
            [52.942519000000004 4.772424000000001]
            [52.94242299999999  4.7877929999999935]
            [52.94961699999999  4.793407000000002]
            [52.95269300000001  4.788003000000003]
            [52.95866000000001  4.804949999999991]
            [52.970844          4.800051999999994]
            [52.971092          4.7655129999999986]])

;; 4.69
(def poly6 [[17.695130000000006 83.26754399999999]
            [17.687502999999992 83.277581]
            [17.686155          83.28386499999999]
            [17.686318          83.291676]
            [17.690638000000007 83.29215099999999]
            [17.693826          83.284854]
            [17.694616999999994 83.28119300000003]
            [17.705155000000005 83.279695]
            [17.706817          83.277716]
            [17.713992000000005 83.27290199999999]
            [17.714690000000004 83.26904200000001]
            [17.70894           83.26161400000001]
            [17.695130000000006 83.26754399999999]])

;; 4209.65
(def poly7 [[19.98498475616872 -91.79134362468773]
            [19.9740615493575  -92.04057221275009]
            [20.03541307963264 -92.14643923513471]
            [19.95465060060891 -92.65622000414476]
            [19.45081690538742 -92.5158885284788]
            [19.66032206271934 -91.76904227116374]
            [19.98498475616872 -91.79134362468773]])

;; 0.61
(def poly8 [[45.61253720661773 13.76805534791982]
            [45.6081753117228  13.77841955800301]
            [45.61382065985224 13.78507279772183]
            [45.61654418007323 13.77955383415076]
            [45.61253720661773 13.76805534791982]])

;; 13.12
(def poly9 [[34.23897057428315 132.5638278053062]
            [34.2616479343     132.524814256]
            [34.260657554      132.515103735]
            [34.2496936292     132.512478625]
            [34.2356858612     132.529040195]
            [34.2223250083     132.51815939]
            [34.2096925593     132.531038298]
            [34.22741372813305 132.5547173987204]
            [34.23331328810249 132.5467411848424]
            [34.23754922026851 132.5505916255833]
            [34.23731528410036 132.5547430421426]
            [34.23449131109498 132.5599368463871]
            [34.23897057428315 132.5638278053062]])

;; 99.24
(def poly10 [[2.988796580801094  101.4027996541245]
              [3.045382892157539 101.3796574154713]
              [3.056749706005817 101.3581881735915]
              [3.080301624046995 101.3491098257278]
              [3.12320917128509  101.3089142074387]
              [3.109044296918606 101.2935373933977]
              [3.064111381047294 101.3359961658755]
              [3.052526194703154 101.3388075055149]
              [3.001018576505298 101.340942216169]
              [2.98341169508234  101.3109068220205]
              [2.922723283922333 101.2789262903789]
              [2.908721947908669 101.2862037356386]
              [2.953388875062547 101.3285886380902]
              [2.988796580801094 101.4027996541245]])

(defn test-area
  [& polygons]
  (doseq [poly polygons]
    (println (format "AREA=%s" (polygon poly)))))


(defn -main
  [& args]
  (let []
    (test-area poly1 poly2 poly3 poly4 poly5 poly6 poly7 poly8 poly9 poly10)))
