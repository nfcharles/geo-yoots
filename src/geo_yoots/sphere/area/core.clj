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



;; -----------
;; - IMPL 2
;; -----------

;; Algorithm
;; -----
;; https://math.stackexchange.com/questions/1435018/change-a-3d-plane-to-xy-plane

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
    (println (format "PROJECTED_VERTICES=%s" pvtx))
    (let [rot-mtx   (rotation-matrix-2 plane-norm)]
      (println (format "ROTATION_MATRIX=%s" rot-mtx))
      (loop [xs pvtx
             acc []]
        (if-let [x (first xs)]
          (let [vtx (rotate rot-mtx x)]
            (println (format "TRANSLATING POINT= (%s -> %s" x vtx))
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


;; ---------

(defn polygon
  [vertices & {:keys [scale]
               :or {scale geo.const/earth-radius}}]
  (* scale (apply-shoelace (shoelace-matrix vertices))))



;; ----------
;; -  Unit Tests
;; ----------

(def poly1 [[ 1.000000  0.000000]
            [ 0.000000  1.000000]
            [-1.000000  0.000000]
            [ 0.000000 -1.000000]])

(def poly2 [[-15.290669  179.159774]
            [-11.469978  179.626362]
            [ -9.559677 -177.255858]
            [-13.678565 -174.857297]
            [-17.228967 -177.598549]])

(defn test-area
  [poly]
  (println (format "AREA=%s" (polygon poly))))


(defn -main
  [& args]
  (let []
    (test-area poly1)
    (test-area poly2)))
