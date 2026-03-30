(ns code_perf_profiler.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [code_perf_profiler.core :as core]))

;; ---------- Unit tests for individual analyzers ----------

(deftest test-count-functions-python
  (let [lines ["def foo():" "    pass" "def bar():" "    pass" "async def baz():" "    pass"]]
    (is (= 3 (core/count-functions lines ".py")))))

(deftest test-count-functions-js
  (let [lines ["function foo() {" "}" "const bar = (x) => {" "}" "function baz(a, b) {" "}"]]
    (is (>= (core/count-functions lines ".js") 2))))

(deftest test-count-functions-clj
  (let [lines ["(defn foo [x]" "  x)" "(defn- bar [y]" "  y)"]]
    (is (= 2 (core/count-functions lines ".clj")))))

(deftest test-avg-function-length
  (let [lines ["def foo():"
               "    x = 1"
               "    y = 2"
               "    return x + y"
               "def bar():"
               "    return 42"]]
    (is (pos? (core/avg-function-length lines ".py")))))

(deftest test-deeply-nested-loops-python
  (let [lines ["for i in range(10):"
               "    for j in range(10):"
               "        for k in range(10):"
               "            print(i, j, k)"]]
    (is (seq (core/detect-deeply-nested-loops lines ".py"))
        "Should detect 3-level nested loops in Python")))

(deftest test-deeply-nested-loops-js
  (let [lines ["function f() {"
               "    for (let i = 0; i < 10; i++) {"
               "        for (let j = 0; j < 10; j++) {"
               "            for (let k = 0; k < 10; k++) {"
               "                console.log(k);"
               "            }"
               "        }"
               "    }"
               "}"]]
    (is (seq (core/detect-deeply-nested-loops lines ".js"))
        "Should detect 3-level nested loops in JS")))

(deftest test-no-nested-loops
  (let [lines ["for i in range(10):"
               "    print(i)"]]
    (is (empty? (core/detect-deeply-nested-loops lines ".py"))
        "Single loop should not trigger")))

(deftest test-repeated-string-concat
  (let [lines ["for item in items:"
               "    result += str(item)"
               "    total += item.name"]]
    (is (seq (core/detect-repeated-string-concat lines ".py"))
        "Should detect += in loop")))

(deftest test-no-string-concat-outside-loop
  (let [lines ["x = 'hello'"
               "x += ' world'"]]
    (is (empty? (core/detect-repeated-string-concat lines ".py"))
        "Concat outside loop should not trigger")))

(deftest test-n-plus-one-python
  (let [lines ["for user_id in ids:"
               "    user = db.objects.get(pk=user_id)"
               "    print(user.name)"]]
    (is (seq (core/detect-n-plus-one lines ".py"))
        "Should detect DB query in loop")))

(deftest test-n-plus-one-js
  (let [lines ["for (const id of ids) {"
               "    const user = db.query('SELECT * FROM users WHERE id = ' + id);"
               "}"]]
    (is (seq (core/detect-n-plus-one lines ".js"))
        "Should detect SQL query in loop")))

(deftest test-sync-io-in-loops-python
  (let [lines ["for f in files:"
               "    data = open(f).read()"]]
    (is (seq (core/detect-sync-io-in-loops lines ".py"))
        "Should detect open() in loop")))

(deftest test-sync-io-in-loops-js
  (let [lines ["for (const f of files) {"
               "    const data = fs.readFileSync(f);"
               "}"]]
    (is (seq (core/detect-sync-io-in-loops lines ".js"))
        "Should detect readFileSync in loop")))

(deftest test-large-collections-python
  (let [lines ["data = list(range(100000))"
               "lines = open('f').readlines()"]]
    (is (seq (core/detect-large-collections-without-streaming lines ".py")))))

;; ---------- Scoring ----------

(deftest test-score-clean-code
  (let [analysis {:deeply-nested-loops    []
                  :repeated-string-concat []
                  :n-plus-one-patterns    []
                  :large-collections      []
                  :sync-io-in-loops       []
                  :avg-function-length    10}]
    (is (= 0 (core/calculate-score analysis))
        "Clean code should score 0")))

(deftest test-score-bad-code
  (let [analysis {:deeply-nested-loops    [{:line 1}]
                  :repeated-string-concat [{:line 2}]
                  :n-plus-one-patterns    [{:line 3}]
                  :large-collections      [{:line 4}]
                  :sync-io-in-loops       [{:line 5}]
                  :avg-function-length    60}]
    (is (> (core/calculate-score analysis) 30)
        "Code with all issues should score high")))

(deftest test-score-capped-at-100
  (let [analysis {:deeply-nested-loops    (repeat 20 {:line 1})
                  :repeated-string-concat (repeat 20 {:line 2})
                  :n-plus-one-patterns    (repeat 20 {:line 3})
                  :large-collections      (repeat 20 {:line 4})
                  :sync-io-in-loops       (repeat 20 {:line 5})
                  :avg-function-length    100}]
    (is (<= (core/calculate-score analysis) 100))))

;; ---------- File analysis ----------

(deftest test-analyze-bad-python-fixture
  (let [result (core/analyze-file "test/fixtures/bad_perf.py")]
    (is (some? result))
    (is (> (:score result) 0) "Bad perf file should have issues")
    (is (pos? (:function-count result)))
    (is (seq (:n-plus-one-patterns result)) "Should detect N+1 queries")
    (is (seq (:sync-io-in-loops result)) "Should detect sync IO in loops")))

(deftest test-analyze-good-python-fixture
  (let [result (core/analyze-file "test/fixtures/good_perf.py")]
    (is (some? result))
    (is (= 0 (:score result)) "Good perf file should score 0")
    (is (= 3 (:function-count result)))))

(deftest test-analyze-bad-js-fixture
  (let [result (core/analyze-file "test/fixtures/bad_perf.js")]
    (is (some? result))
    (is (> (:score result) 0) "Bad JS file should have issues")
    (is (seq (:n-plus-one-patterns result)))))

(deftest test-analyze-clean-js-fixture
  (let [result (core/analyze-file "test/fixtures/clean.js")]
    (is (some? result))
    (is (= 0 (:score result)) "Clean JS file should score 0")))

(deftest test-analyze-unsupported-extension
  (is (nil? (core/analyze-file "test/fixtures/readme.txt"))
      "Unsupported extensions should return nil"))

;; ---------- Directory scanning ----------

(deftest test-scan-fixtures-dir
  (let [results (core/scan-directory "test/fixtures")]
    (is (>= (count results) 4) "Should find all fixture files")
    (is (= (:file (first results))
            (-> (filter #(> (:score %) 0) results) first :file))
        "Results should be sorted by score descending")))

;; ---------- Output formatting ----------

(deftest test-format-text-output
  (let [results [{:file "test.py" :score 45 :lines 100 :function-count 5
                  :avg-function-length 20.0
                  :deeply-nested-loops [{:line 10}]
                  :repeated-string-concat []
                  :n-plus-one-patterns [{:line 15}]
                  :large-collections []
                  :sync-io-in-loops []}]
        output (core/format-text results 0)]
    (is (str/includes? output "test.py"))
    (is (str/includes? output "45/100"))
    (is (str/includes? output "N+1"))
    (is (str/includes? output "Deeply nested"))))

(deftest test-format-text-threshold-filters
  (let [results [{:file "a.py" :score 10 :lines 10 :function-count 1
                  :avg-function-length 5.0
                  :deeply-nested-loops [] :repeated-string-concat []
                  :n-plus-one-patterns [] :large-collections [] :sync-io-in-loops []}
                 {:file "b.py" :score 50 :lines 10 :function-count 1
                  :avg-function-length 5.0
                  :deeply-nested-loops [{:line 1}] :repeated-string-concat []
                  :n-plus-one-patterns [] :large-collections [] :sync-io-in-loops []}]
        output (core/format-text results 30)]
    (is (not (str/includes? output "a.py")) "Below-threshold file should be excluded")
    (is (str/includes? output "b.py"))))

(deftest test-format-json-output
  (let [results [{:file "test.py" :score 30 :lines 50 :function-count 3
                  :avg-function-length 15.0
                  :deeply-nested-loops [{:line 5 :depth 3 :text "for x"}]
                  :repeated-string-concat []
                  :n-plus-one-patterns []
                  :large-collections []
                  :sync-io-in-loops []}]
        output (core/format-json results 0)
        parsed (cheshire.core/parse-string output true)]
    (is (= 1 (get-in parsed [:summary :total-files])))
    (is (= 1 (count (:results parsed))))))

(deftest test-format-edn-output
  (let [results [{:file "test.py" :score 30 :lines 50 :function-count 3
                  :avg-function-length 15.0
                  :deeply-nested-loops [] :repeated-string-concat []
                  :n-plus-one-patterns [] :large-collections [] :sync-io-in-loops []}]
        output (core/format-edn results 0)
        parsed (read-string output)]
    (is (= 1 (get-in parsed [:summary :total-files])))))

;; ---------- Edge cases ----------

(deftest test-empty-file
  (let [lines []]
    (is (= 0 (core/count-functions lines ".py")))
    (is (= 0 (core/avg-function-length lines ".py")))
    (is (empty? (core/detect-deeply-nested-loops lines ".py")))))

(deftest test-file-extension
  (is (= ".py" (core/file-extension "foo/bar.py")))
  (is (= ".js" (core/file-extension "app.test.js")))
  (is (nil? (core/file-extension "Makefile"))))
