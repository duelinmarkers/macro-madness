(ns macro-madness.core-test
  (:use clojure.test macro-madness.core))

(def expected-map {:k1 "kay one" :k2 "kay two"})

(defmap0 my-map0 :k1 "kay one" :k2 "kay two")
(defmap my-map :k1 "kay one" :k2 "kay two")
(defmap2 my-map2 :k1 "kay one" :k2 "kay two")

(deftest simple-macros
  (testing "defmap0"
    (is (= my-map0 expected-map))
    (is (= (macroexpand '(defmap0 foo :a "Aye"))
           '(def foo (hash-map :a "Aye")))))
  (testing "defmap"
    (is (= my-map expected-map))
    (is (= (macroexpand '(defmap foo :a "Aye"))
           '(def foo {:a "Aye"}))))
  (testing "defmap2"
    (is (= my-map2 expected-map))
    (is (= (macroexpand '(defmap2 foo :a "Aye"))
           '(def foo (clojure.core/hash-map :a "Aye"))))))
