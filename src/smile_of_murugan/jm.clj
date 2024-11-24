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

(def TERMINAL-WS-REGEX #"\s+$")

(defn- reformat-newline-in-italics
  "Newlines inside an italics region in Markdown lead to
   improper rendering. They empirically confuse the renderer.
   Instead, add more Markdown italics delimiters around the newline
   to prevent the problem (thereby excising the newline from
   the region of italics styling)."
  [s]
  (let [newline-search-regex #"(\s*)(\n)(\s*)(\S)"]
    (string/replace s newline-search-regex #(str (nth % 1) "*\n*" (nth % 3) (nth % 4)))))

(defn- swap-order-of-terminal-whitespace-in-italics
  "find terminal whitespace in the string coming after the end delimiter
   of italicized text and swap the order
   of the close italic symbol with the terminal whitespace."
  [s]
  (if (re-find TERMINAL-WS-REGEX s)
    (string/replace s TERMINAL-WS-REGEX #(str "*" %1))
    (str s "*")))

(defn format-stylized-text-substring-for-markdown
  "Return the Markdown-formatted output for stylized substring text.
   Input = substring text that was marked as italicized style.
   When extracting maximal length substrings of the original text
   that are stylized (italicized), it is not enough to merely apply
   the Markdown italics syntax on the substring.
   This is because the OCR tool includes whitespace at the end of
   such substrings, and having whitespace between the text and the
   symbol is confusing to the Markdown parser & renderer, in practice.
   Therefore, we need to find such terminal whitespace and swap the order
   of the close italic symbol with the terminal whitespace.
   This problem also occurs for whitespace _in the middle of the substring_."
  [preformatted-str]
  (let [is-str-all-non-letters (re-matches #"[^\p{L}\p{N}]*" preformatted-str)]
    (if is-str-all-non-letters
      preformatted-str
      (-> preformatted-str
          reformat-newline-in-italics
          swap-order-of-terminal-whitespace-in-italics 
          (->> (str "*"))))))

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

(defn get-word-index-scores
  [resp]
  (let [text (get resp "text")
        text-length (count text)
        context-length 20
        word-index-scores (for [page (get resp "pages")
                                token (get page "tokens")]
                            (let [confidence (get-in token ["layout" "confidence"])
                                  first-segment (-> (get-in token ["layout" "textAnchor" "textSegments"])
                                                    first)
                                  last-segment (-> (get-in token ["layout" "textAnchor" "textSegments"])
                                                   last)
                                  start-index (Integer/parseInt (get first-segment "startIndex" "0"))
                                  end-index (Integer/parseInt (get last-segment "endIndex" "0"))
                                  substring (subs text start-index end-index)]
                              {:confidence confidence
                               :start-index start-index
                               :end-index end-index
                               :substring substring
                               :context (subs text
                                              (max 0 (- start-index context-length))
                                              (min text-length (+ end-index context-length)))}))]
    word-index-scores))

(def SPELLING-CORRECTIONS
  {"paṇar" "pāṇar"
   "canrōr" "cāṉṟōr"})

"cāṉṟōr" ;; Elango edition - NFD form
"cāṉṟōr" ;; Internet inspired - NFC form




(defn filter-word-index-scores
  [word-index-scores]
  (let [no-punctuation (remove #(re-matches #"[\p{Space}\p{Punct}]+" (:substring %)) word-index-scores)]
    ;; TODO: remove all words of a high confidence score that match the English dictionary
    no-punctuation))

(defn docai-json-to-md
  "Convert the DocAI response JSON directly into Markdown"
  [resp-json]
  (let [resp (json/parse-string resp-json)
        text (get-stylized-text resp)
        lines (string/split-lines text)
        unhyphenated-lines (transform/join-hyphenated-line-ends lines)
        lines-with-paragraphs (insert-paragraph-lines unhyphenated-lines)]
    lines-with-paragraphs))
