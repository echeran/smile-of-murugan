(ns smile-of-murugan.jm-test
  (:require [smile-of-murugan.dictionary :as dict]
            [smile-of-murugan.jm :as jm]
            [smile-of-murugan.transform :as transform]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [smile-of-murugan.dictionary :as d]))

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
    (comment TODO: look for adjacent lines that wrap like this:
             "called *manai,
akam*, il")
    (comment TODO: allow for hyphenated compound word to be on one line
             by modifying the unhyphenation fn accordingly)
    (is false)))