(ns geo-yoots.sphere.impl.inclusion-test
  (:require [clojure.test :refer :all]
            [geo-yoots.sphere.impl.inclusion :refer :all]))


;; ---- Inclusion Fixtures

(def polygon-1-testcase
  (hash-map
    :plane   [54.355198 10.164557]
    :polygon [[54.310769 10.129421]
              [54.309937 10.135439]
              [54.316093 10.143296]
              [54.317051 10.150138]
              [54.326334 10.172375]
              [54.324386 10.185774]
              [54.330768 10.188743]
              [54.333148 10.179165]
              [54.361172 10.183925]
              [54.364926 10.202201]
              [54.393815 10.198791]
              [54.390464 10.152578]
              [54.375627 10.156783]
              [54.370442 10.123081]
              [54.366189 10.120979]
              [54.362752 10.1402]
              [54.350562 10.136483]
              [54.342714 10.152149]
              [54.310769 10.129421]]
    :in      [[54.356222 10.160379]
              [54.367320 10.194962]
              [54.326997 10.154361]
              [54.311375 10.133376]
              [54.327469 10.185386]
              [54.368393 10.124422]
              [54.392739 10.197563]]
    :out     [[54.394247 10.194452]
              [54.391750 10.199868]
              [54.370384 10.111040]
              [54.361521 10.063705]
              [54.444150 10.234952]]))


;; ----

(def polygon-2-testcase
  (hash-map
    :plane   [29.992134 -90.450516]
    :polygon [[29.993095 -90.460828]
              [29.992001 -90.454835]
              [29.990196 -90.446007]
              [29.99019  -90.43206]
              [29.994351 -90.432192]
              [29.994385 -90.434352]
              [29.995379 -90.445602]
              [29.996012 -90.451396]
              [29.99729  -90.45873]
              [29.997392 -90.458726]
              [29.997392 -90.45888]
              [29.997426 -90.459535]
              [29.992934 -90.460871]
              [29.993095 -90.460828]]
    :in      [[29.993428 -90.442797]
              [29.994479 -90.455238]]
    :out     [[29.998512 -90.446112]
              [29.995766 -90.429642]
              [30.000350 -90.467376]]))


;; ---

(def polygon-3-testcase
  (hash-map
    :plane   [29.980272 -93.880814]
    :polygon [[29.977193 -93.873439]
              [29.981075 -93.871994]
              [29.984797 -93.891251]
              [29.979695 -93.893399]
              [29.977193 -93.873439]]
    :in      [[29.979534 -93.881330]
              [29.979767 -93.873637]]
    :out     [[29.983746 -93.894129]
              [29.983196 -93.880776]
              [29.978697 -93.872460]]))


;; ---

(def polygon-4-testcase
  (hash-map
    :plane   [52.957531 4.787157]
    :polygon [[52.971092 4.765513]
              [52.95706 4.764809]
              [52.956077 4.769635]
              [52.942519 4.772424]
              [52.94246 4.772424]
              [52.942283 4.772424]
              [52.942048 4.772521]
              [52.941989 4.772521]
              [52.942431 4.772424]
              [52.942423 4.787793]
              [52.949617 4.793407]
              [52.952693 4.788003]
              [52.95866 4.80495]
              [52.970844 4.800052]
              [52.971092 4.765513]]
    :in      [[52.955609 4.790501]
              [52.944242 4.785970]
              [52.957911 4.767963]]
    :out     [[52.952342 4.790257]
              [52.955226 4.768825]
              [52.957576 4.803784]]))


;; ---

(def polygon-5-testcase
  (hash-map
    :plane   [17.694231 83.277054]
    :polygon [[17.69513 83.267544]
              [17.687503 83.277581]
              [17.686155 83.283865]
              [17.686318 83.291676]
              [17.690638 83.292151]
              [17.693826 83.284854]
              [17.693827 83.284853]
              [17.694617 83.281193]
              [17.705155 83.279695]
              [17.706817 83.277716]
              [17.713992 83.272902]
              [17.71469 83.269042]
              [17.70894 83.261614]
              [17.69513 83.267544]]
    :in      [[17.704905 83.279060]
              [17.695118 83.269114]
              [17.694113 83.281570]]
    :out     [[17.694846 83.281376]
              [17.701886 83.280566]
              [17.688819 83.292343]
              [17.711433 83.264397]]))

;; ---

(def polygon-6-testcase
  (hash-map
    :plane   [53.504833 8.514603]
    :polygon [[53.530342 8.557599]
              [53.516751 8.526092]
              [53.5043 8.503495]
              [53.491814 8.492118]
              [53.479464 8.483088]
              [53.46614 8.478799]
              [53.465589 8.49133]
              [53.474873 8.491064]
              [53.490824 8.501526]
              [53.501435 8.514013]
              [53.524488 8.563445]
              [53.530342 8.557599]]
    :in      [[53.474897 8.490436]
              [53.503861 8.511143]
              [53.524373 8.562518]]
    :out     [[53.474778 8.491399]]))


;; ---

(def polygon-7-testcase
  (hash-map
    :plane   [19.68 -92.087997]
    :polygon [[19.984985, -91.791344]
              [19.974062, -92.040572]
              [20.035413, -92.146439]
              [19.954651, -92.65622]
              [19.450817, -92.515889]
              [19.660322, -91.769042]
              [19.984985, -91.791344]]
    :in      [[19.945228 -91.840841]
              [19.637810 -92.193672]
              [19.942504 -92.643283]
              [19.952741 -92.653931]
              [19.954061 -92.655662]]
    :out     [[19.976894 -92.039441]
              [19.428962 -92.527213]
              [19.808805 -93.276270]
              [19.282991 -91.691915]
              [19.955085 -92.656947]]))


;; --- Function Helpers

(defn -test-point-inclusion
  [direction tc expected]
  (doseq [pt (direction tc)]
    (is (= (point-in-polygon? pt (:plane tc) (:polygon tc)) expected))))

(defn test-point-in-polygon
  [tc]
  (-test-point-inclusion :in tc true))

(defn test-point-out-polygon
  [tc]
  (-test-point-inclusion :out tc false))


;; --- Test Functions

(deftest triangle-split-test
  (testing "Simple vertices"
    (= (triangle-splits (range 5)) [[0 1 2] [0 2 3] [0 3 4]])))


(deftest point-inclusion-test
  (testing "Inside"
    (test-point-in-polygon polygon-1-testcase)
    (test-point-in-polygon polygon-2-testcase)
    (test-point-in-polygon polygon-3-testcase)
    (test-point-in-polygon polygon-4-testcase)
    (test-point-in-polygon polygon-5-testcase)
    (test-point-in-polygon polygon-6-testcase)
    (test-point-in-polygon polygon-7-testcase))

  (testing "Outside"
    (test-point-out-polygon polygon-1-testcase)
    (test-point-out-polygon polygon-2-testcase)
    (test-point-out-polygon polygon-3-testcase)
    (test-point-out-polygon polygon-4-testcase)
    (test-point-out-polygon polygon-5-testcase)
    (test-point-out-polygon polygon-6-testcase)
    (test-point-out-polygon polygon-7-testcase)))
