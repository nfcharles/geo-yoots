(ns geo-yoots.sphere.centroid-test
  (:require [clojure.test :refer :all]
            [geo-yoots.test-util :as test.util]
            [geo-yoots.sphere.centroid :refer :all]))




;; --- Test Helpers

(defn -test-centroid
  [pts expected]
  (is (= (points pts) expected)))



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
