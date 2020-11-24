(ns geo-yoots.sphere.centroid-test
  (:require [clojure.test :refer :all]
            [geo-yoots.test-util :as test.util]
            [geo-yoots.sphere.centroid :refer :all]))


(deftest centroid-polygon-test
  (testing "Location 1 - Polygon"
    (test.util/compare-latlon
      (polygon [[-15.290669  179.159774]
                     [-11.469978  179.626362]
                     [ -9.559677 -177.255858]
                     [-13.678565 -174.857297]
                     [-17.228967 -177.598549]
                     [-13.836214 -177.062038]
                     [-12.340054 -178.316774]])
      [-13.350482379748883 -178.04316885079348])))
