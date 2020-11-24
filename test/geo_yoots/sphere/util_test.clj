(ns geo-yoots.sphere.util-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as mtx]
            [geo-yoots.test-util :as test.util]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.sphere.util :refer :all]))



;; --- Test Helpers

(defn -test-latlon
  [[lat lon]]
  (let [x (latlon->x [lat lon])
        y (latlon->y [lat lon])
        z (latlon->z [lat lon])]
    (is (= (test.util/round-float lat 5) (test.util/round-float (cartesian->lat [x y z]) 5)))
    (is (= (test.util/round-float lon 5) (test.util/round-float (cartesian->lon [x y z]) 5)))))

(defn -test-spherical
  [latlon rho theta phi]
  (let [cart (latlon->cartesian latlon)]
    (is (= rho   (cartesian->rho cart)))
    (is (= theta (test.util/round-float (Math/toDegrees (cartesian->theta cart)) 1)))
    (is (= phi   (test.util/round-float (Math/toDegrees (cartesian->phi cart)) 1)))))


;; ---- Tests

(deftest latlon-test
  (testing "Lat Lon 1"
    (-test-latlon [0.0 0.0]))

  (testing "Lat Lon 2"
    (-test-latlon [17.688132 83.299026])))

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


;; --- Test Helpers

(defn -test-centroid
  [pts expected]
  (test.util/compare-latlon (centroid pts) expected))


(deftest centroid-points-test
  (testing "Location 1 - Points"
    (-test-centroid [[ 1.0  0.0]
                     [ 0.0  1.0]
                     [-1.0  0.0]
                     [ 0.0 -1.0]] [0.0 0.0]))

  (testing "Location 2 - Points"
    (-test-centroid [[-21.1333 -175.2   ]  ; Tonga
                     [ -8.53333 179.2167]] ; Tuvalu
                    [-14.850146658102771 -178.07333636886855]))

  (testing "Location 3 - Points(Polygon)"
    (-test-centroid [[-15.290669  179.159774]
                     [-11.469978  179.626362]
                     [ -9.559677 -177.255858]
                     [-13.678565 -174.857297]
                     [-17.228967 -177.598549]
                     [-13.836214 -177.062038]
                     [-12.340054 -178.316774]]
                    [-13.350482394698734 -178.04316885779255])))
