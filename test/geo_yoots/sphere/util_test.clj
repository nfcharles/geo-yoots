(ns geo-yoots.sphere.util-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as mtx]
            [geo-yoots.test-util :as test.util]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.sphere.util :refer :all])) ;;:as geo.sphere.util]))


;; --- Test Helpers

(defn -test-latlon
  [[lat lon]]
  (let [x (latlon->x [lat lon])
        y (latlon->y [lat lon])
        z (latlon->z [lat lon])]
    (is (= (test.util/round-float lat 5) (test.util/round-float (cartesian->lat [x y z]) 5)))
    (is (= (test.util/round-float lon 5) (test.util/round-float (cartesian->lon [x y z]) 5)))))

(defn -test-centroid
  [pts expected]
  (is (= (centroid pts) expected)))

(defn -test-spherical
  [latlon rho theta phi]
  (let [cart (latlon->cartesian latlon)]
    (is (= rho   (cartesian->rho cart)))
    (is (= theta (test.util/round-float (Math/toDegrees (cartesian->theta cart)) 1)))
    (is (= phi   (test.util/round-float (Math/toDegrees (cartesian->phi cart)) 1)))))

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
  (is (= (ortho-plane-projection pt plane unit-v) expected)))



;; ---- Tests


(deftest latlon-test
  (testing "Lat Lon 1"
    (-test-latlon [0.0 0.0]))

  (testing "Lat Lon 2"
    (-test-latlon [17.688132 83.299026])))

(deftest centroid-test
  (testing "Location 1"
    (-test-centroid [[ 1.0  0.0]
                     [ 0.0  1.0]
                     [-1.0  0.0]
                     [ 0.0 -1.0]] [0.0 0.0]))

  (testing "Location 2"
    (-test-centroid [[-21.1333 -175.2   ]  ; Tonga
                     [ -8.53333 179.2167]] ; Tuvalu
                    [-14.850146658102771 -178.07333636886855])))

(deftest spherical-test
  (testing "Coordinates (45,0)"
    (-test-spherical [45     0] 1.0     0.0   45.0))
  (testing "Coordinates (45, -90)"
    (-test-spherical [45   -90] 1.0    -90.0  45.0))
  (testing "Coordinates (45, -180)"
    (-test-spherical [45  -180] 1.0   -180.0  45.0))
  (testing "Coordinates (17.5, -135)"
    (-test-spherical [17.5  -135] 1.0 -135.0  72.5))
  (testing "Coordinates (-90, -180)"
    (-test-spherical [-90 -180] 1.0   -180.0 180.0)))

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
