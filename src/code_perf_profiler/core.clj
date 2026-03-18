(ns code_perf_profiler.core
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [cheshire.core :as json]))

(def supported-extensions
  #{".py" ".js" ".ts" ".java" ".rb" ".php" ".clj" ".cljs"})

(defn file-extension [path]
  (let [name (str (fs/file-name path))]
    (when-let [idx (str/last-index-of name ".")]
      (subs name idx))))

;; ---------- Analyzers ----------

(defn count-functions [lines ext]
  (let [patterns (case ext
                   ".py"          [#"^\s*(async\s+)?def\s+\w+"]
                   (".js" ".ts")  [#"^\s*function\s+\w+"
                                   #"^\s*(export\s+)?(const|let|var)\s+\w+\s*=\s*(async\s+)?\("
                                   #"^\s*(export\s+)?(const|let|var)\s+\w+\s*=\s*(async\s+)?\w+\s*=>"
                                   #"^\s*(async\s+)?\w+\s*\([^)]*\)\s*\{"]
                   ".java"        [#"^\s*(public|private|protected|static|\s)+[\w<>\[\]]+\s+\w+\s*\("]
                   ".rb"          [#"^\s*def\s+\w+"]
                   ".php"         [#"^\s*(public|private|protected|static|\s)*function\s+\w+"]
                   (".clj" ".cljs") [#"^\s*\(defn-?\s+"]
                   [])]
    (count (filter (fn [line] (some #(re-find % line) patterns)) lines))))

(defn detect-function-starts [lines ext]
  (let [patterns (case ext
                   ".py"          [#"^\s*(async\s+)?def\s+\w+"]
                   (".js" ".ts")  [#"^\s*function\s+\w+" #"^\s*(export\s+)?(const|let|var)\s+\w+\s*=\s*(async\s+)?\("]
                   ".java"        [#"^\s*(public|private|protected|static|\s)+[\w<>\[\]]+\s+\w+\s*\("]
                   ".rb"          [#"^\s*def\s+\w+"]
                   ".php"         [#"^\s*(public|private|protected|static|\s)*function\s+\w+"]
                   (".clj" ".cljs") [#"^\s*\(defn-?\s+"]
                   [])]
    (keep-indexed (fn [idx line]
                    (when (some #(re-find % line) patterns) idx))
                  lines)))

(defn avg-function-length [lines ext]
  (let [starts (detect-function-starts lines ext)
        n      (count lines)]
    (if (empty? starts)
      0
      (let [boundaries (map vector starts (concat (rest starts) [n]))
            lengths    (map (fn [[s e]] (- e s)) boundaries)]
        (double (/ (reduce + lengths) (count lengths)))))))

(defn- loop-pattern [ext]
  (case ext
    ".py"            [#"^\s*for\s+" #"^\s*while\s+"]
    (".js" ".ts")    [#"\bfor\s*\(" #"\bwhile\s*\(" #"\.forEach\(" #"\.map\(" #"\.reduce\("]
    ".java"          [#"\bfor\s*\(" #"\bwhile\s*\("]
    ".rb"            [#"\.(each|times|loop)\b" #"\b(while|until|for)\b"]
    ".php"           [#"\bfor\s*\(" #"\bwhile\s*\(" #"\bforeach\s*\("]
    (".clj" ".cljs") [#"\((?:doseq|dotimes|loop|for)\b"]
    []))

(defn detect-deeply-nested-loops [lines ext]
  (let [lp (loop-pattern ext)]
    (if (= ext ".py")
      ;; Python: indentation-based nesting
      (let [loop-entries (keep-indexed
                          (fn [idx line]
                            (when (some #(re-find % line) lp)
                              {:line (inc idx)
                               :indent (count (take-while #(= \space %) line))
                               :text (str/trim line)}))
                          lines)]
        (let [entries (vec loop-entries)]
          (for [i     (range (count entries))
                :let  [base (:indent (nth entries i))
                       nested (take-while #(> (:indent %) base) (subvec entries (inc i)))
                       depth  (if (empty? nested) 1
                                  (inc (count (distinct (filter #(> % base) (map :indent nested))))))]
                :when (>= depth 3)]
            {:line (:line (nth entries i)) :depth depth :text (:text (nth entries i))})))
      ;; Brace languages: track brace depth
      (let [brace-depth (atom 0)
            results     (atom [])]
        (doseq [[idx line] (map-indexed vector lines)]
          (let [opens  (count (filter #(= \{ %) line))
                closes (count (filter #(= \} %) line))]
            (swap! brace-depth + opens)
            (when (some #(re-find % line) lp)
              (swap! results conj {:line (inc idx) :depth @brace-depth :text (str/trim line)}))
            (swap! brace-depth - closes)))
        (filter #(>= (:depth %) 3) @results)))))

(defn- in-loop-detector
  "Run `pred` on each line; when inside a loop body (within `window` lines), collect findings."
  [lines ext pred window]
  (let [lp         (loop-pattern ext)
        findings   (atom [])
        in-loop?   (atom false)
        loop-start (atom 0)]
    (doseq [[idx line] (map-indexed vector lines)]
      (when (some #(re-find % line) lp)
        (reset! in-loop? true)
        (reset! loop-start (inc idx)))
      (when (and @in-loop? (pred line))
        (swap! findings conj {:line (inc idx) :loop-start @loop-start :text (str/trim line)}))
      (when (and @in-loop? (> (- idx (dec @loop-start)) window))
        (reset! in-loop? false)))
    @findings))

(defn detect-repeated-string-concat [lines ext]
  (let [concat-pats (case ext
                      ".py"          [#"\+=" #"=\s*\w+\s*\+\s*[\"']" #"\.format\(" #"%\s*\("]
                      (".js" ".ts")  [#"\+=" #"=\s*\w+\s*\+\s*[\"'`]"]
                      ".java"        [#"\+=" #"\.concat\(" #"=\s*\w+\s*\+\s*\""]
                      ".rb"          [#"\+=" #"<<\s*[\"']" #"\.concat\("]
                      ".php"         [#"\.=" #"=\s*\w+\s*\.\s*[\"']"]
                      (".clj" ".cljs") [#"\(str\s+"]
                      [])]
    (in-loop-detector lines ext
                      (fn [line] (some #(re-find % line) concat-pats))
                      20)))

(defn detect-n-plus-one [lines ext]
  (let [db-pats [#"(?i)\b(select|insert|update|delete)\b.*\b(from|into|where|set)\b"
                 #"(?i)\.query\(" #"(?i)\.execute\(" #"(?i)\.find\(" #"(?i)\.findOne\("
                 #"(?i)\.findBy\w*\(" #"(?i)\.fetch\(" #"(?i)\.select\(" #"(?i)\.where\("
                 #"(?i)cursor\." #"(?i)\.raw\(" #"(?i)\.exec\("
                 #"(?i)ActiveRecord" #"(?i)\.objects\." #"(?i)\.get\(.*pk"]]
    (in-loop-detector lines ext
                      (fn [line] (some #(re-find % line) db-pats))
                      30)))

(defn detect-large-collections-without-streaming [lines ext]
  (let [pats (case ext
               ".py"          [#"\blist\(range\(\d{4,}" #"\.readlines\(\)" #"sorted\(.*\brange\(\d{4,}"]
               (".js" ".ts")  [#"Array\(\d{4,}\)" #"\.split\(.*\)\.map\(" #"JSON\.parse\(.*\)\.(map|filter)\("]
               ".java"        [#"\.collect\(Collectors\.toList\(\)\)" #"\.toArray\(" #"new\s+ArrayList.*addAll"]
               ".rb"          [#"\.to_a\b" #"\.readlines\b"]
               ".php"         [#"file_get_contents\(" #"json_decode\("]
               (".clj" ".cljs") [#"\(into\s+\[\]" #"\(vec\s+" #"\(doall\s+"]
               [])]
    (keep-indexed (fn [idx line]
                    (when (some #(re-find % line) pats)
                      {:line (inc idx) :text (str/trim line)}))
                  lines)))

(defn detect-sync-io-in-loops [lines ext]
  (let [io-pats (case ext
                  ".py"          [#"\bopen\(" #"\.read\(" #"\.write\(" #"\brequests\." #"\burllib\b"]
                  (".js" ".ts")  [#"fs\.readFileSync" #"fs\.writeFileSync" #"\bfetch\(" #"\baxios\." #"http\.get\("]
                  ".java"        [#"new\s+File\b" #"\.read\(" #"\.write\(" #"HttpClient" #"new\s+URL\("]
                  ".rb"          [#"File\.(read|open)" #"Net::HTTP" #"open-uri"]
                  ".php"         [#"file_get_contents\(" #"fopen\(" #"curl_exec\(" #"fread\("]
                  (".clj" ".cljs") [#"\(slurp\b" #"\(spit\b" #"clj-http" #"\(io/reader\b"]
                  [])]
    (in-loop-detector lines ext
                      (fn [line] (some #(re-find % line) io-pats))
                      30)))

;; ---------- Scoring ----------

(defn calculate-score [analysis]
  (let [nested   (min 25 (* 8  (count (:deeply-nested-loops analysis))))
        concat   (min 15 (* 5  (count (:repeated-string-concat analysis))))
        nplus1   (min 25 (* 12 (count (:n-plus-one-patterns analysis))))
        lgcoll   (min 10 (* 3  (count (:large-collections analysis))))
        syncio   (min 15 (* 7  (count (:sync-io-in-loops analysis))))
        fnlen    (let [avg (:avg-function-length analysis)]
                   (cond (> avg 50) 10 (> avg 30) 5 (> avg 20) 2 :else 0))]
    (min 100 (+ nested concat nplus1 lgcoll syncio fnlen))))

;; ---------- File / directory scanning ----------

(defn analyze-file [path]
  (let [ext (file-extension path)]
    (when (and ext (supported-extensions ext))
      (try
        (let [content (slurp (str path))
              lines   (str/split-lines content)
              analysis {:file                   (str path)
                        :lines                  (count lines)
                        :function-count         (count-functions lines ext)
                        :avg-function-length    (avg-function-length lines ext)
                        :deeply-nested-loops    (vec (detect-deeply-nested-loops lines ext))
                        :repeated-string-concat (vec (detect-repeated-string-concat lines ext))
                        :n-plus-one-patterns    (vec (detect-n-plus-one lines ext))
                        :large-collections      (vec (detect-large-collections-without-streaming lines ext))
                        :sync-io-in-loops       (vec (detect-sync-io-in-loops lines ext))}]
          (assoc analysis :score (calculate-score analysis)))
        (catch Exception e
          {:file (str path) :error (.getMessage e) :score 0})))))

(defn scan-directory [dir]
  (->> (fs/glob dir "**")
       (filter fs/regular-file?)
       (filter #(supported-extensions (file-extension %)))
       (map analyze-file)
       (remove nil?)
       (sort-by :score >)
       vec))

;; ---------- Output formatting ----------

(defn format-text [results threshold]
  (let [filtered (filter #(>= (:score %) threshold) results)]
    (if (empty? filtered)
      (format "No files with performance risk score >= %d" threshold)
      (str/join "\n"
        (concat
          ["Performance Analysis Results"
           (apply str (repeat 60 "="))]
          (mapcat
            (fn [r]
              (concat
                [(format "\nFile: %s" (:file r))
                 (format "  Score: %d/100 | Lines: %d | Functions: %d | Avg length: %.0f"
                         (:score r) (:lines r) (:function-count r) (double (:avg-function-length r)))]
                (when (seq (:deeply-nested-loops r))
                  [(format "  [WARN] Deeply nested loops: %d" (count (:deeply-nested-loops r)))])
                (when (seq (:repeated-string-concat r))
                  [(format "  [WARN] String concat in loops: %d" (count (:repeated-string-concat r)))])
                (when (seq (:n-plus-one-patterns r))
                  [(format "  [CRIT] N+1 query patterns: %d" (count (:n-plus-one-patterns r)))])
                (when (seq (:large-collections r))
                  [(format "  [INFO] Large collections without streaming: %d" (count (:large-collections r)))])
                (when (seq (:sync-io-in-loops r))
                  [(format "  [WARN] Synchronous I/O in loops: %d" (count (:sync-io-in-loops r)))])))
            filtered)
          [(str "\n" (apply str (repeat 60 "=")))]
          [(format "Files analyzed: %d | Files with issues: %d" (count results) (count filtered))])))))

(defn summarize-findings [findings]
  (mapv #(select-keys % [:line :depth :loop-start]) findings))

(defn format-json [results threshold]
  (let [filtered (filter #(>= (:score %) threshold) results)]
    (json/generate-string
      {:summary {:total-files      (count results)
                 :files-with-issues (count filtered)
                 :threshold         threshold}
       :results (mapv (fn [r]
                        (-> r
                            (update :deeply-nested-loops summarize-findings)
                            (update :repeated-string-concat summarize-findings)
                            (update :n-plus-one-patterns summarize-findings)
                            (update :large-collections summarize-findings)
                            (update :sync-io-in-loops summarize-findings)))
                      filtered)}
      {:pretty true})))

(defn format-edn [results threshold]
  (let [filtered (filter #(>= (:score %) threshold) results)]
    (pr-str {:summary {:total-files      (count results)
                       :files-with-issues (count filtered)
                       :threshold         threshold}
             :results (vec filtered)})))

;; ---------- CLI ----------

(def cli-spec
  {:dir       {:desc "Directory to scan" :default "." :alias :d}
   :format    {:desc "Output format: text, json, edn" :default "text" :alias :f}
   :threshold {:desc "Minimum risk score to report (0-100)" :default 0 :coerce :int :alias :t}
   :help      {:desc "Show help" :alias :h :coerce :boolean}})

(defn -main [& args]
  (let [opts (cli/parse-opts args {:spec cli-spec})]
    (when (:help opts)
      (println "code-perf-profiler — Analyze code for performance risks")
      (println)
      (println (cli/format-opts {:spec cli-spec}))
      (System/exit 0))
    (let [dir       (:dir opts)
          fmt       (:format opts)
          threshold (:threshold opts)]
      (when-not (fs/exists? dir)
        (binding [*out* *err*]
          (println (format "Error: directory '%s' does not exist" dir)))
        (System/exit 2))
      (let [results (scan-directory dir)
            output  (case fmt
                      "json" (format-json results threshold)
                      "edn"  (format-edn results threshold)
                      (format-text results threshold))]
        (println output)
        (let [issues (filter #(> (:score %) 0) results)]
          (System/exit (if (seq issues) 1 0)))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
