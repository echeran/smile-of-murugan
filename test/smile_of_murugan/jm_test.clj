(ns smile-of-murugan.jm-test
  (:require [smile-of-murugan.dictionary :as dict]
            [smile-of-murugan.jm :as jm]
            [smile-of-murugan.transform :as transform]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [smile-of-murugan.dictionary :as d]
            [clojure.string :as str]))

(deftest json-parse-and-convert-test
  (testing "Context of the test assertions"
    (d/load-dictionary)
    (let [json-file-name "sample/docai-doc-ocr.json"
          json-file (fs/file json-file-name)
          json-str (slurp json-file)
          md-lines (jm/docai-json-to-md json-str)
          test-out-file-name "temp.md"]
      (spit test-out-file-name (string/join \newline md-lines)))
    (d/close-dictionary)))

(deftest json-style-test-1
  (testing "Extract italicized word Cankam"
    (let [json-file-name "sample/docai-doc-ocr.json"
          json-file (fs/file json-file-name)
          json-str (slurp json-file)
          resp (json/parse-string json-str)
          text (get resp "text")]
      (let [word1 (subs text 877 884)
            word2 (subs text 884 890)
            word3 (subs text 890 892)]
        (prn "word1:" word1)
        (prn "word2:" word2)
        (prn "word3:" word3)))))

(deftest json-style-test-2
  (testing "Extract italicized word élite"
    (let [json-file-name "sample/docai-doc-ocr.json"
          json-file (fs/file json-file-name)
          json-str (slurp json-file)
          resp (json/parse-string json-str)
          text (get resp "text")]
      (let [word1 (subs text 1018 1021)
            word2 (subs text 1021 1026)
            word3 (subs text 1026 1028)]
        (prn "(class text):" (class text))
        (prn "word1:" word1)
        (prn "word2:" word2)
        (prn "word3:" word3)))))

(deftest json-style-test-3
  (testing "Extract italicized set of words `love-poetry, `"
    (let [json-file-name "sample/docai-doc-ocr.json"
          json-file (fs/file json-file-name)
          json-str (slurp json-file)
          resp (json/parse-string json-str)
          text (get resp "text")]
      (let [word1 (subs text 8151 8164)]
        (prn "(class text):" (class text))
        (prn "word1:" word1)))))

(deftest unhyphenated-lines-test
  (testing "Remove hyphenation from lines wrapped around a line break with a hypen"
    (d/load-dictionary)
    (let [text-str "without any determina-
tion of their"
          lines (string/split-lines text-str)
          unhyphenated-lines (transform/join-hyphenated-line-ends lines)
          expected ["without any determination"
                    "of their"]]
      (is (= expected unhyphenated-lines)))
    (d/close-dictionary)))

(deftest json-style-test-4
  (testing "Fix stylizing text that wraps a newline" 
    #_(comment "TODO: look for adjacent lines that wrap like this:"
             "called *manai,
akam*, il")
    #_(comment "TODO: allow for hyphenated compound word to be on one line
             by modifying the unhyphenation fn accordingly")
    (let [input "manai,
akam"
          expected "*manai,*
*akam*"
          actual (jm/format-stylized-text-substring-for-markdown input)]
      (is (= expected actual)))))

(deftest json-style-test-5
  (testing "Fix stylizing text that wraps a newline - Example 2"
    (let [input "élite\n"
          expected "*élite*\n"
          actual (jm/format-stylized-text-substring-for-markdown input)]
      (is (= expected actual)))))

;; If we see this, we can worry about it
#_(deftest json-style-test-6
  (testing "Fix stylizing text that wraps a newline - Example 3" 
    (let [input "manai
,"
          expected "*manai*
,"
          actual (jm/format-stylized-text-substring-for-markdown input)]
      (is (= expected actual)))))

;; TODO: start here next time
(deftest json-style-dehyphentation-combo-test
    (testing "Fix a stylized word that is hyphentated and wraps a newline"
      (d/load-dictionary)
      (let [input "Tolkāppi-
yam"
            expected "*Tolkāppiyam*"
            actual (-> input
                       jm/format-stylized-text-substring-for-markdown
                       string/split-lines
                       transform/join-hyphenated-line-ends)]
        (is (= expected actual)))
      (d/close-dictionary)))

#_(deftest response-confidence-score-test
  (testing "Examine the regions where the confidence score is low"
    (let [json-file-name "sample/docai-doc-ocr.json"
          json-file (fs/file json-file-name)
          json-str (slurp json-file)
          resp (json/parse-string json-str)
          text (get resp "text")]
      (let [word1-99 (subs text 3919 3924)
            word2-80 (subs text 3924 3930)
            word3-53 (subs text 3930 3931)
            word4-98 (subs text 3931 3939)]
        (prn "word1 with 99 confidence:" word1-99)
        (prn "word2 with 80 confidence:" word2-80)
        (prn "word3 with 53 confidence:" word3-53)
        (prn "word4 with 98 confidence:" word4-98)))))

(defn print-word-index-score
  [word-index-score]
  (println (str (pr-str (:substring word-index-score))
                \tab
                (pr-str (:context word-index-score)))))

(deftest inspect-low-confidence-tokens
  (testing "Print out the lowest N confidence scores from tokens in response object"
    (d/load-dictionary)
    (let [json-file-name "sample/docai-doc-ocr.json"
          json-file (fs/file json-file-name)
          json-str (slurp json-file)
          resp (json/parse-string json-str)
          word-index-scores (jm/get-word-index-scores resp)
          ordered-word-index-scores (->> word-index-scores
                                         (jm/filter-word-index-scores)
                                         (sort-by (comp str/lower-case :substring)))
          cutoff-n 500]
      (println "lowest" cutoff-n "confidence score words:")
      (run! print-word-index-score (take cutoff-n ordered-word-index-scores)))
    (d/close-dictionary)))