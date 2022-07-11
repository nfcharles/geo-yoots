(ns geo-yoots.sphere.area.core
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [geo-yoots.sphere.transformations :as geo.sphere.xform]
            [geo-yoots.sphere.rotation :as geo.sphere.rot]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))




(def X 0)
(def Y 1)
(def Z 2)

;; TODO: deprecated for matrix method
(defn vertices->projection-plane2
  [vertices]
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
        cent       (geo.sphere.util/centroid uniq-verts)

        ;; projection plane pt
        cv         (geo.sphere.xform/latlon->vector cent)

        ;; projection plane unit normal
        unit-nv    (geo.sphere.xform/unit-normal-vector cent)
        ]

    (loop [xs  uniq-verts
           acc []]
      (if-let [x (first xs)]
        (recur (rest xs) (conj acc (geo.sphere.xform/vector-ortho-plane-projection (geo.sphere.xform/latlon->vector x) cv unit-nv)))
        (mtx/matrix acc)))))

(defn polygon->normal
  [vtxs]
  (let []
    (mtx/cross (geo.sphere.xform/a->b-vector (.getRow vtxs 1) (.getRow vtxs 2))
               (geo.sphere.xform/a->b-vector (.getRow vtxs 1) (.getRow vtxs 3)))))


;; =================
;; - Area of Polygon
;; -----------------
;; - Transform projected polgyon (centroid projection plane) to XY plane and apply shoelace algorithm
;; -
;; - https://en.wikipedia.org/wiki/Shoelace_formula
;; ---

(defn shoelace-matrix
  [vertices]
  (let [;;pvtx        (geo.sphere.xform/vertices->projection-plane vertices)
        pvtxs        (vertices->projection-plane2 vertices)
        plane-norm   (polygon->normal pvtxs) #_(geo.sphere.xform/polygon->normal pvtxs)]  ;; normal vector of polygon projection plane
    #_(println (format "PROJECTED_VERTICES=%s" pvtx))
    (let [;;rot-mtx   (geo.sphere.rot/xy-plane-rotation plane-norm)
          rot-alt   (geo.sphere.xform/rotation-matrix plane-norm)]
      #_(println (format "ROTATION_MATRIX=%s\nALT_MATRIX=%s" rot-mtx rot-alt))

      (mtx/transpose (mtx.op/* geo.const/earth-radius (geo.sphere.xform/rotate rot-alt (mtx/transpose pvtxs)))))))

(defn apply-shoelace
  [pts]
  (let [n (.rowCount pts)]
    (loop [i   0
           acc 0]
      (if (< i n)
        (let [i_idx   (mod i n)
              i+1_idx (mod (inc i) n)
              x_i     (.get pts i_idx   X)
              y_i+1   (.get pts i+1_idx Y)
              x_i+1   (.get pts i+1_idx X)
              y_i     (.get pts i_idx   Y)]
          #_(println (format "%s * %s - %s * %s" x_i y_i+1 x_i+1 y_i))
          (recur (inc i)
                 ( + acc (- (* x_i y_i+1) (* x_i+1 y_i)))))
        (/ (Math/abs acc) 2)))))



;; ====================
;; -  Area Functions  -
;; ====================

(defn polygon
  [vertices]
  (apply-shoelace (shoelace-matrix vertices)))

(defn circle
  [radius]
  (* Math/PI (geo.util/sq radius)))
