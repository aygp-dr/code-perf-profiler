(ns code-perf-profiler.specs
  "Data specs for code-perf-profiler (https://clojure.org/guides/spec).
  Function specs (s/fdef) live next to each defn in code_perf_profiler.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]))

;; Generators are built inside fns, never in top-level defs:
;; clojure.spec.gen.alpha loads test.check on first use, and the JVM runtime
;; classpath (deps.edn :deps) has no test.check.

;; --- Inputs ---

;; The extensions core/supported-extensions accepts; analyze-file only runs
;; the analyzers for these.
(s/def ::ext #{".py" ".js" ".ts" ".java" ".rb" ".php" ".clj" ".cljs"})

(def ^:private code-fragments
  ["for i in range(10):" "    for j in items:" "        for k in data:" "while queue:"
   "    result += str(item)" "    user = db.objects.get(pk=uid)" "    data = open(f).read()"
   "data = list(range(100000))" "lines = fh.readlines()" "def load(path):"
   "async def fetch(url):" "    return x" "function f(a) {" "  for (let i = 0; i < n; i++) {"
   "    db.query('SELECT * FROM users WHERE id = ' + id);" "    fs.readFileSync(p);" "  }" "}"
   "items.forEach(x => {" "const g = (a) => {" "(defn h [x]" "  (doseq [y xs]" "    (slurp y))"
   "public static int m(int a) {" "xs.each do |y|" "foreach ($rows as $r) {" "$s .= 'x';" ""])

(defn- gen-line []
  (gen/one-of [(gen/elements code-fragments)
               (gen/fmap (fn [[n s]] (str (str/join (repeat n " ")) s))
                         (gen/tuple (gen/choose 0 16) (gen/elements code-fragments)))
               (gen/string-alphanumeric)]))

;; One line of source text, as produced by clojure.string/split-lines.
(s/def ::line-text
  (s/with-gen (s/and string? #(not (re-find #"[\r\n]" %))) gen-line))

(s/def ::source-lines (s/coll-of ::line-text :kind vector? :gen-max 40))

(s/def ::detector-args (s/cat :lines ::source-lines :ext ::ext))

;; A relative or absolute file path, as a string or java.nio.file.Path.
(defn- gen-path-string []
  (gen/fmap (fn [[dirs base ext]]
              (str/join "/" (conj dirs (cond-> base ext (str "." ext)))))
            (gen/tuple (gen/vector (gen/not-empty (gen/string-alphanumeric)) 0 3)
                       (gen/not-empty (gen/string-alphanumeric))
                       (gen/one-of [(gen/return nil)
                                    (gen/elements ["py" "js" "ts" "java" "rb" "php" "clj" "cljs" "txt"])
                                    (gen/string-alphanumeric)]))))

(s/def ::path-like
  (s/with-gen (s/or :string (s/and string? seq)
                    :path #(instance? java.nio.file.Path %))
    gen-path-string))

;; --- Findings ---

(s/def ::line pos-int?)
(s/def ::depth int?)
(s/def ::loop-start pos-int?)
(s/def ::text string?)

;; What the detectors return.
(s/def ::nested-loop (s/and (s/keys :req-un [::line ::depth ::text])
                            #(<= 3 (:depth %))))
(s/def ::loop-finding (s/keys :req-un [::line ::loop-start ::text]))
(s/def ::line-finding (s/keys :req-un [::line ::text]))

;; What scoring and the formatters read. They only count findings and pick
;; :line/:depth/:loop-start, and the unit tests pass bare {:line n} maps.
(s/def ::finding (s/keys :req-un [::line] :opt-un [::depth ::loop-start ::text]))
(s/def ::findings (s/coll-of ::finding :kind sequential? :gen-max 5))

(defn findings-within-input?
  "s/fdef :fn for the detect-* fns: every finding points at a 1-based line
  of the input, and a loop finding's loop starts at or before that line."
  [{{:keys [lines]} :args ret :ret}]
  (every? (fn [{:keys [line loop-start]}]
            (and (<= 1 line (count lines))
                 (or (nil? loop-start) (<= 1 loop-start line))))
          ret))

;; --- Per-file results ---

(s/def ::file string?)
(s/def ::lines nat-int?)
(s/def ::function-count nat-int?)
(s/def ::avg-function-length
  (s/with-gen (s/and number? #(not (neg? %)))
    #(gen/one-of [(gen/choose 0 120)
                  (gen/double* {:min 0 :max 120 :NaN? false :infinite? false})])))
(s/def ::deeply-nested-loops ::findings)
(s/def ::repeated-string-concat ::findings)
(s/def ::n-plus-one-patterns ::findings)
(s/def ::large-collections ::findings)
(s/def ::sync-io-in-loops ::findings)
(s/def ::score (s/int-in 0 101))
(s/def ::error (s/nilable string?))

;; calculate-score's input.
(s/def ::analysis
  (s/keys :req-un [::deeply-nested-loops ::repeated-string-concat ::n-plus-one-patterns
                   ::large-collections ::sync-io-in-loops ::avg-function-length]))

;; One entry of scan-directory's result: a scored analysis, or the
;; {:file :error :score 0} that analyze-file returns for an unreadable file.
(s/def ::analysis-result
  (s/merge ::analysis (s/keys :req-un [::file ::lines ::function-count ::score])))
(s/def ::error-result (s/keys :req-un [::file ::error ::score]))
(s/def ::file-result (s/or :analysis ::analysis-result :error ::error-result))
(s/def ::results (s/coll-of ::file-result :kind sequential? :gen-max 5))

(s/def ::threshold (s/with-gen int? #(gen/choose -5 105)))

;; --- CLI option table (the babashka.cli :spec map) ---

(s/def ::desc string?)
(s/def ::default (s/or :string string? :int int?))
(s/def ::alias simple-keyword?)
(s/def ::coerce #{:boolean :string :int :long :double :keyword :symbol})
(s/def ::cli-option (s/keys :req-un [::desc] :opt-un [::default ::alias ::coerce]))
(s/def ::cli-spec (s/map-of simple-keyword? ::cli-option))
