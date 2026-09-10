(ns code-perf-profiler.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [code_perf_profiler.core :as sut]
            [code-perf-profiler.specs :as specs]))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
(def ^:private side-effecting
  #{`sut/analyze-file `sut/scan-directory `sut/-main})

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace 'code_perf_profiler.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/source-lines ::specs/path-like ::specs/finding ::specs/findings
             ::specs/analysis ::specs/file-result ::specs/results ::specs/cli-spec]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(deftest real-values-conform
  (testing "lookup tables"
    (is (every? (partial s/valid? ::specs/ext) sut/supported-extensions))
    (is (s/valid? ::specs/cli-spec sut/cli-spec)))
  (testing "the fixture files"
    (is (s/valid? ::specs/file-result (sut/analyze-file "test/fixtures/bad_perf.py")))
    (is (s/valid? ::specs/results (sut/scan-directory "test/fixtures"))))
  (testing "a real detector result"
    (let [lines ["for i in range(10):"
                 "    for j in range(10):"
                 "        for k in range(10):"
                 "            print(i, j, k)"]]
      (is (s/valid? (s/coll-of ::specs/nested-loop)
                    (sut/detect-deeply-nested-loops lines ".py"))))))
