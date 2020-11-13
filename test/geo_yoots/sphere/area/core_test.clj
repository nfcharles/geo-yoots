(ns geo-yoots.sphere.area.core-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as mtx]  
            [geo-yoots.test-util :as test.util]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.area.core :as geo.sphere.area]))



;; ----
;; - Rotation Matrix Tests
;; ----





;; ----
;; - Shoelace Aglo Tests
;; ----

;; Testcase Source: https://en.wikipedia.org/wiki/Shoelace_formula

(def mtx1
  (hash-map
    :mtx  (mtx/matrix
            [[2  4]
             [3 -8]
             [1  2]])
    :area 7.0))


(def mtx2
  (hash-map
    :mtx  (mtx/matrix
            [[ 3  4]
             [ 5 11]
             [12  8]
             [ 9  5]
             [ 5  6]])
    :area 30.0))

(def mtx3
  (hash-map
    :mtx  (mtx/matrix
            [[ 2  7]
             [10  1]
             [ 8  6]
             [11  7]
             [ 7 10]])
    :area 32.0))


(deftest shoelace-apply-test
  (let [cases [mtx1 mtx2 mtx3]]
    (testing "Matrices"
      (doseq [[i case] (map list (range (count cases)) cases)]
        (testing (format "Case: %d" i)
          (is (= (geo.sphere.area/apply-shoelace (:mtx case)) (:area case))))))))


;; ----
;; - TODO: Add Area Tests - Move from area.core package and formalize.
;; ---
