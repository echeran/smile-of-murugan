(ns smile-of-murugan.jm-test
  (:require [smile-of-murugan.jm :as jm]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]
            [babashka.fs :as fs]))

(deftest name-test
  (testing "Context of the test assertions"
    (let [json-file-name "sample/docai-doc-ocr.json"
          json-file (fs/file json-file-name)
          json-str (slurp json-file)
          md-lines (jm/docai-json-to-md json-str)
          ]
      (->> md-lines
           (take 50)
           (run! println))
      (is (= true false)))))
