(ns geo-yoots.sphere.orientation-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as mtx]
            [geo-yoots.test-util :as test.util]
            [geo-yoots.sphere.orientation :refer :all]))


;; TODO: more tests

(def RIGHT   -1)
(def LEFT     1)
(def COLINEAR 0)


;; -- left | right tests

(def mtx-right
  (mtx/matrix [[0 0 2]
               [0 1 2]
               [1 1 1]]))

(def mtx-left
  (mtx/matrix [[0 0 -2]
               [0 1 -2]
               [1 1  1]]))


;; -- colinear tests

(def mtx-colinear-cases
  [(mtx/matrix [[0 0 0]
                [0 2 4]
                [1 1 1]])

   (mtx/matrix [[0 2 4]
                [0 2 4]
                [1 1 1]])

   (mtx/matrix [[0  2  4]
                [0 -2 -4]
                [1  1  1]])])


;; ---- pt | line orientation

(def pt-line-cases
  [{:line [[49.94080259513916 -0.4983458924349693]
           [49.78889032649092 -0.43668119115714]]
    :pt   [50.410894 0.572014]
    :out  LEFT}
   {:line [[49.94080259513916 -0.4983458924349693]
           [49.78889032649092 -0.43668119115714]]
    :pt   [50.217519 1.657981]
    :out  LEFT}
   {:line [[49.94080259513916 -0.4983458924349693]
           [49.78889032649092 -0.43668119115714]]
    :pt   [50.410895098307634 -0.5720125245671284]
    :out  LEFT}
   {:line [[49.94080259513916 -0.4983458924349693]
           [49.78889032649092 -0.43668119115714]]
    :pt   [50.21752003959513  -1.6579794309908493]
    :out  RIGHT}

   {:line [[29.00671313207889 32.94778832709038]
           [28.95424370618806 32.79132620519571]]
    :pt   [29.597533238195677 32.520689964294434]
    :out  RIGHT}

   {:line [[29.00671313207889 32.94778832709038]
           [28.95424370618806 32.79132620519571]]
    :pt   [28.59706708120612 33.06344032287598]
    :out  LEFT}

   {:line [[29.00671313207889 32.94778832709038]
           [28.95424370618806 32.79132620519571]]
    :pt   [30.23886982969937 32.54075288772583]
    :out  RIGHT}

   {:line [[3.817729773265194 99.42852237160083]
           [4.015436916168416 99.79900769223298]]
    :pt   [4.179428093972673 99.55746173858643]
    :out  LEFT}

   {:line [[3.817729773265194 99.42852237160083]
           [4.015436916168416 99.79900769223298]]
    :pt   [1.2422992865520495 104.05657768249513]
    :out  RIGHT}

   {:line [[3.817729773265194 99.42852237160083]
           [4.015436916168416 99.79900769223298]]
    :pt   [4.166851396835301 100.1681121396713]
    :out  RIGHT}

   {:line [[3.817729773265194 99.42852237160083]
           [4.015436916168416 99.79900769223298]]
    :pt   [6.148582837324728 95.66137075424193]
    :out  LEFT}

   {:line [[9.091587006679919 -79.85203819082183]
           [9.190078613046811 -79.76953303564144]]
    :pt   [9.415728789091315 -79.93579387664795]
    :out  LEFT}

   {:line [[9.091587006679919 -79.85203819082183]
           [9.190078613046811 -79.76953303564144]]
    :pt   [8.833567162791129 -79.52088832855225]
    :out  RIGHT}

   {:line [[9.091587006679919 -79.85203819082183]
           [9.190078613046811 -79.76953303564144]]
    :pt   [9.023415565235524 -79.62133169174194]
    :out  RIGHT}

   {:line [[9.091587006679919 -79.85203819082183]
           [9.190078613046811 -79.76953303564144]]
    :pt   [9.180908538251858 -79.85118627548218 ]
    :out  LEFT}])



(deftest left-orientation-test
  (testing "Left Orientation Test"
    (is (= (orientation mtx-left) LEFT))))

(deftest right-orientation-test
  (testing "Right Orientation Test"
    (is (= (orientation mtx-right) RIGHT))))

(deftest colinear-orientation-test
  (testing "Colinear Test"
    (doseq [-mtx mtx-colinear-cases]
      (is (= (orientation -mtx) COLINEAR)))))

;; ----

(deftest pt-line-orientation-test
  (testing "Pt Line Orientation Test"
    (doseq [case pt-line-cases]
      (let [{:keys [line pt out]} case]
        (is (= (point-line-orientation pt line) out))))))
