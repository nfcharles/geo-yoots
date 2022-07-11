(ns geo-yoots.sphere.rotation
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.consts]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.stats :as mtx.stats]
            [clojure.core.matrix.operators :as mtx.op]))


(mtx/set-current-implementation :vectorz)

(def xyv (mtx/matrix [0 0 1]))
(def xzv (mtx/matrix [0 1 0]))
(def yzv (mtx/matrix [1 0 0]))

#_(defn theta
  "Find angle of rotation from normal vector and plane vector"
  [normv rotv]
  (Math/acos (mtx.stats/cosine-similarity normv rotv)))

(defn theta
  "Find angle of rotation from normal vector and plane vector"
  [normv rotv]
  (Math/acos (mtx.stats/cosine-similarity normv rotv)))

(defn xy-plane-rotation
  [normv]
  (let [_t (theta normv xyv)
        cos-t (Math/cos _t)
        sin-t (Math/sin _t)]
    (mtx/matrix
      [[cos-t        sin-t 0]
       [(* -1 sin-t) cos-t 0]
       [0            0     1]])))

(defn xz-plane-rotation
  [normv]
  (let [_t (theta normv xzv)
        cos-t (Math/cos _t)
        sin-t (Math/sin _t)]
    (mtx/matrix
      [[cos-t        0 sin-t]
       [0            1     0]
       [(* -1 sin-t) 0 cos-t]])))

(defn yz-plane-rotation
  [normv]
  (let [_t (theta normv yzv)
        cos-t (Math/cos _t)
        sin-t (Math/sin _t)]
    (mtx/matrix
      [[1           0      0]
       [0       cos-t  sin-t]
       [0 (* -1 sin-t) cos-t]])))
