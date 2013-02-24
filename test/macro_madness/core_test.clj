(ns macro-madness.core-test
  (:use clojure.test macro-madness.core))

(use-fixtures :each (fn [f] (binding [*ns* (the-ns 'macro-madness.core-test)] (f)))) ;; Makes macroexpand tests work.

(deftest simple-defmap-macros

  (def expected-map {:k1 "one" :k2 "two"})

  (testing "defmap0"
    (defmap0 my-map0 :k1 "one" :k2 "two")
    (is (= my-map0 expected-map))
    (is (= '(def foo (clojure.core/hash-map :a "Aye")) (macroexpand '(defmap0 foo :a "Aye")))))

  (testing "defmap"
    (defmap my-map :k1 "one" :k2 "two")
    (is (= my-map expected-map))
    (is (= '(def foo {:a "Aye"}) (macroexpand '(defmap foo :a "Aye")))))

  (testing "defmap2"
    (defmap2 my-map2 :k1 "one" :k2 "two")
    (is (= my-map2 expected-map))
    (is (= '(def foo (clojure.core/hash-map :a "Aye")) (macroexpand '(defmap2 foo :a "Aye"))))))

(deftest try-times-macro-and-fn

  (defn inc-and-throw [an-atom]
    (swap! an-atom inc)
    (throw (RuntimeException. (str "on attempt " @an-atom))))

  (testing "macro and fn both try the given number of times"
    (let [attempts (atom 0)
          caught (try
                   (try-times 3 (inc-and-throw attempts))
                   (catch RuntimeException e e))]
      (is (= 3 @attempts))
      (is (= "on attempt 3" (.getMessage caught))))


    (let [attempts (atom 0)
          caught (try
                   (try-times* 3 #(inc-and-throw attempts))
                   (catch RuntimeException e e))]
      (is (= 3 @attempts))
      (is (= "on attempt 3" (.getMessage caught)))))

  (testing "the fn can be composed, while the macro cannot"
    (let [try-twice (partial try-times* 2)
          attempts (atom 0)]
      (try
        (try-twice #(inc-and-throw attempts))
        (catch RuntimeException e nil))
      (is (= 2 @attempts)))

    (comment ; this won't compile
      (let [try-twice (partial try-times 2) ;; CompilerException "Can't take value of a macro"
            attempts (atom 0)]
        (try
          (try-twice (inc-and-throw attempts))
          (catch RuntimeException e nil))
        (is (= 2 @attempts))))))
