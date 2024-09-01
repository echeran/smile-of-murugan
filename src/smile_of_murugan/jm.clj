(ns smile-of-murugan.jm
  (:require [cheshire.core :as json]
            [clojure.string :as string]))

(def END-OF-PARAGRAPH-CHAR-LIMIT 55)

(defn- is-end-of-para?
  [line]
  (and (string/ends-with? line ".")
       (> END-OF-PARAGRAPH-CHAR-LIMIT (count line))))

(defn insert-paragraph-lines
  [lines]
  (loop [result []
        ;;  curr-line nil
         remaining-lines lines]
    (let [next-line (first remaining-lines)]
      (if-not (seq next-line)
        result
        (if (is-end-of-para? next-line)
          (recur (-> result
                     (conj next-line)
                     (conj ""))
                 (rest remaining-lines))
          (recur (-> result
                     (conj next-line))
                 (rest remaining-lines)))))))

(defn docai-json-to-md
  [resp-json]
  (let [resp (json/parse-string resp-json)
        text (get resp "text")
        lines (string/split-lines text)
        lines-with-paragraphs (insert-paragraph-lines lines)]
    lines-with-paragraphs))
