(ns smile-of-murugan.jm
  (:require [cheshire.core :as json]
            [clojure.string :as string]
            [smile-of-murugan.transform :as transform]))

(def END-OF-PARAGRAPH-CHAR-LIMIT 55)

(defn- is-end-of-para?
  "Helper function that describes the heuristic for inferring when the
   end of a paragraph has been reached."
  [line]
  (and (re-find #"\.(\*)?$" line) ;;(string/ends-with? line ".")
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

(defn- is-token-italic?
  [token]
  (get-in token ["styleInfo" "italic"]))

(defn- format-stylized-text-substring-for-markdown
  "Return the Markdown-formatted output for stylized substring text.
   When extracting maximal length substrings of the original text
   that are stylized (italicized), it is not enough to merely apply
   the Markdown italics syntax on the substring.
   This is because the OCR tool includes whitespace at the end of
   such substrings, and having whitespace between the text and the
   symbol is confusing to the Markdown parser & renderer, in practice.
   Therefore, we need to find such terminal whitespace and swap the order
   of the close italic symbol with the terminal whitespace.
   This problem may also occur for whitespace _in the middle of the substring_,
   but we haven't yet identified such cases so far in our testing."
  [preformatted-str]
  (let [is-str-all-non-letters (re-matches #"[^\p{L}\p{N}]*" preformatted-str)]
    (if is-str-all-non-letters
      preformatted-str
      (let [terminal-ws-regex #"\s+$"
            str-with-close-italic-symbol (if (re-find terminal-ws-regex preformatted-str)
                                           (string/replace preformatted-str terminal-ws-regex #(str "*" %1))
                                           (str preformatted-str "*"))]
        (str "*" str-with-close-italic-symbol)))))

(defn- stylize-tokens
  "partition token seq based on whether they are stylized (italicized) or not,
   then concatenate the partitions' respective text together,
   and return the seq of concatenated text"
  [text tokens]
  (let [partitioned-tokens (partition-by is-token-italic? tokens)]
    (for [partition partitioned-tokens]
      (let [first-token (first partition)
            first-segment (-> (get-in first-token ["layout" "textAnchor" "textSegments"])
                              first)
            is-italic (is-token-italic? first-token)
            last-token (last partition)
            last-segment (-> (get-in last-token ["layout" "textAnchor" "textSegments"])
                             last)
            start-index (Integer/parseInt (get first-segment "startIndex" "0"))
            end-index (Integer/parseInt (get last-segment "endIndex" "0"))
            substr (subs text start-index end-index)]
        (if is-italic
          (format-stylized-text-substring-for-markdown substr)
          substr)))))

(defn- get-stylized-text
  "Convert the DocAI response JSON into the Markdown version
   of the text in which the styles (ex: italics) have already
   been applied"
  [resp]
  (let [text (get resp "text")
        tokens (for [page (get resp "pages")
                     token (get page "tokens")]
                 token)
        stylized-tokens (stylize-tokens text tokens)]
    (string/join stylized-tokens)))

(defn docai-json-to-md
  "Convert the DocAI response JSON directly into Markdown"
  [resp-json]
  (let [resp (json/parse-string resp-json)
        text (get-stylized-text resp)
        lines (string/split-lines text)
        unhyphenated-lines (transform/join-hyphenated-line-ends lines)
        lines-with-paragraphs (insert-paragraph-lines unhyphenated-lines)]
    lines-with-paragraphs))
