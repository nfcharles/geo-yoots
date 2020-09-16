(ns geo-yoots.util-test
  (:require [clojure.test :refer :all]
            [geo-yoots.test-util :as test.util]
            [geo-yoots.util.core :as geo.util]))



(deftest polyline-edges-test
  (testing "Generate polyline edges 1"
    (let [xs [1 2 3 4 5]]
      (is (= (geo.util/gen-polyline-edges xs) [[1 2] [2 3] [3 4] [4 5]]))))

  (testing "Generate polyline edges 1.1"
    (let [xs [1 2]]
      (is (= (geo.util/gen-polyline-edges xs) [[1 2]]))))

  (testing "Generate polyline edges 2"
    (let [xs [[1 2] [3 4] [5 6] [7 8]]]
      (is (= (geo.util/gen-polyline-edges xs) [[[1 2] [3 4]] [[3 4] [5 6]] [[5 6] [7 8]]])))))


(deftest polgyon-edges-test
  (testing "Generate polygon edges 1"
    (let [xs [1 2 3 4 5]]
      (is (= (geo.util/gen-polygon-edges xs) [[1 2] [2 3] [3 4] [4 5] [5 1]]))))

  (testing "Generate polygon edges 1.1"
    (let [xs [1 2 3]]
      (is (= (geo.util/gen-polygon-edges xs) [[1 2] [2 3] [3 1]]))))

  (testing "Generate polygon edges 1.1"
    (let [xs [[1 2] [3 4] [5 6] [7 8]]]
      (is (= (geo.util/gen-polygon-edges xs) [[[1 2] [3 4]] [[3 4] [5 6]] [[5 6] [7 8]] [[7 8] [1 2]]])))))


(deftest unique-vertices-test
  (testing "Unique vertex"
    (let [xs [[1 1] [2 2] [3 3] [1 1]]]
      (is (= (geo.util/ensure-unique-vertices xs) (rest xs))))

    (let [xs [[1 1] [2 2] [3 3]]]
      (is (= (geo.util/ensure-unique-vertices xs) xs)))))
