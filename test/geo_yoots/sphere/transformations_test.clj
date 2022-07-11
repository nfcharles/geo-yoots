(ns geo-yoots.sphere.transformations-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as mtx]
            [geo-yoots.test-util :as test.util]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.sphere.transformations :refer :all]))


;; --- Test Helpers

(defn -test-unit-vector
  [latlon]
  (is (= (test.util/round-float (mtx/magnitude (unit-normal-vector latlon)) 1) 1.0)))

(defn -test-normal-vector
  [latlon]
  (is (= (test.util/round-float (mtx/magnitude (normal-vector latlon)) 1) (test.util/round-float geo.const/earth-radius 1))))

(defn -test-vector
  [a b expected]
  (is (= (a->b-vector a b) expected)))

(defn -test-distance-to-plane
  [pt plane unit-v expected]
  (is (= (distance-to-plane pt plane unit-v) expected)))

(defn -test-ortho-plane-projection
  [pt plane unit-v expected]
  (is (= (vector-ortho-plane-projection pt plane unit-v) expected)))



;; ---- Tests


(deftest vector-test
  (testing "Unit vector"
    (-test-unit-vector [-21.1333 -175.2])
    (-test-unit-vector [ -8.53333 179.2167])
    (-test-unit-vector [45 -179])
    (-test-unit-vector [0 0]))

  (testing "Normal vector"
    (-test-normal-vector [0 0]))

  (testing "Vector"
    (let [p (mtx/matrix [ 3 1 -8])
          q (mtx/matrix [ 4 4  1])
          r (mtx/matrix [-5 7  2])]
      (-test-vector p q (mtx/matrix [ 1.0 3.0  9.0]))
      (-test-vector p r (mtx/matrix [-8.0 6.0 10.0])))))


(deftest plane-distance-test
  (testing "Distance to plane"
    ;; Q(3, 8, -1)
    ;; 4(x + 2) - (y + 5) + 2z = 0
    (let [p (mtx/matrix [-2 -5  0])  ;; point in plane
          q (mtx/matrix [ 3  8 -1])  ;; point off plane
          n (mtx/matrix [ 4 -1  2])] ;; normal vector
      (-test-distance-to-plane q p n 1.091089451179962))))


(defn normalize
  [xs]
  (mtx/normalise (mtx/matrix xs)))

(deftest ortho-plane-projection-test
  (testing "Plane projection"
    ;; Q(5, -6, 3)
    ;; 3(x - 0) - 2(y - 0) + (z - 2) = 0
    ;; Proj(-1, -2, 1)
    (let [p   (mtx/matrix [0  0  2]) ;; point in plane
          q   (mtx/matrix [5 -6  3]) ;; point off plane
          unv (normalize [3 -2  1])] ;; unit normal vector
      (-test-ortho-plane-projection q p unv (mtx/matrix [-1.0 -2.0 1.0]))))

  (testing "Plane projection 2"
    (let [p   (mtx/matrix [-2 -5  0]) ;; point in plane
          q   (mtx/matrix [ 3  8 -1]) ;; point off plane
          unv (normalize [4 -1  2])]  ;; unit normal vector
      (-test-ortho-plane-projection q p unv (mtx/matrix [2.047619047619047,8.238095238095239,-1.4761904761904765])))))

(deftest polygon-partitions-test
  (testing "Simple vertices"
    (= (partition-polygon (range 5)) [[0 1 2] [0 2 3] [0 3 4]])))
