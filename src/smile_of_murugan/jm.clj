(ns smile-of-murugan.jm
  (:require [cheshire.core :as json]
            [clojure.string :as string]
            [clojure.string :as str]))

(def END-OF-PARAGRAPH-CHAR-LIMIT 55)

(defn- is-end-of-para?
  "Helper function that describes the heuristic for inferring when the
   end of a paragraph has been reached."
  [line]
  (and (string/ends-with? line ".")
       (> END-OF-PARAGRAPH-CHAR-LIMIT (count line))))

(defn insert-paragraph-lines
  "Programmatically infer and insert empty lines, which
   have the effect of creating paragraph boundaries
   within the output Markdown text"
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

(defn- token->stylized-string
  [text token] 
  (let [is-italic (get-in token ["styleInfo" "italic"])
        substr (str/join (for [text-segment (get-in token ["layout" "textAnchor" "textSegments"])]
                           (let [start-index (Integer/parseInt (get text-segment "startIndex" "0"))
                                 end-index (Integer/parseInt (get text-segment "endIndex" "0"))] 
                             (subs text start-index end-index))))] 
    (if is-italic
      (str "*" substr "*")
      substr)))

(defn- get-stylized-text
  "Convert the DocAI response JSON into the Markdown version
   of the text in which the styles (ex: italics) have already
   been applied"
  [resp]
  (let [text (get resp "text")
        tokens (for [page (get resp "pages")
                     token (get page "tokens")]
                 token)]
    (str/join (map #(token->stylized-string text %) tokens))))

(defn docai-json-to-md
  "Convert the DocAI response JSON directly into Markdown"
  [resp-json]
  (let [resp (json/parse-string resp-json)
        text (get-stylized-text resp)
        lines (string/split-lines text)
        lines-with-paragraphs (insert-paragraph-lines lines)]
    lines-with-paragraphs))
