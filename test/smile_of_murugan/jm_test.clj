(ns smile-of-murugan.jm-test
  (:require [smile-of-murugan.jm :as jm]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]
            [babashka.fs :as fs]
            [cheshire.core :as json]))

(deftest json-parse-and-convert-test
  (testing "Context of the test assertions"
    (let [json-file-name "sample/docai-doc-ocr.json"
          json-file (fs/file json-file-name)
          json-str (slurp json-file)
          md-lines (jm/docai-json-to-md json-str)
          ]
      (->> md-lines
           (take 150)
           (run! println))
      (is (= true false)))))

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
        (prn "word3:" word3))
      (is (= true false)))))

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
        (prn "word3:" word3))
      (is (= true false)))))
