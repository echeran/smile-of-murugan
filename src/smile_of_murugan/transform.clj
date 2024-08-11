(ns smile-of-murugan.transform
  (:require
   [clojure.string :as string]
   [smile-of-murugan.dictionary :as d]))

(def STOP-PHRASES
  "Phrases to remove from the extracted text when they appear as a full line"
  #{"The Smile of Murugan"
    "Introduction"})

(defn parse-int-or-nil
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception _
      nil)))

(defn filter-line
  "Return nil if the line should be removed. Else, return the line as is.

  file-num: the 1-based file number
  idx: the 0-based index of the line
  within the set of lines
  from the extracted text of a page
  line: text of line"
  [file-num idx line]
  (when-not (and (< idx 10)
                 (or (and (not (contains? #{13 14 15} file-num)) ;; skip exclusion of titles in TOC
                          (contains? STOP-PHRASES line))
                     (when-let [num (parse-int-or-nil line)]
                       (or (and (<= file-num 226)
                                (= 18 (- file-num num)))
                           (and (= 107 file-num) ;; this is a fluke inexplicable extra artifact from the scan
                                (= 88 num))
                           (and (<= 243 file-num)
                                (= 34 (- file-num num)))))))
    line))

(defn join-hyphenated-line-ends
  [lines]
  (let [lines-split-into-words (loop [result []
                                      curr-line-words nil
                                      remaining-lines lines]
                                 (if-not (seq remaining-lines)
                                   (conj result curr-line-words)
                                   (if-not (seq curr-line-words)
                                     (recur result
                                            (string/split (or (first remaining-lines) "") #"\s+")
                                            (rest remaining-lines))
                                     (let [next-line (first remaining-lines)
                                           next-line-words (string/split (or next-line "") #"\s+")
                                           last-word-1 (last curr-line-words)
                                           first-word-2 (first next-line-words)
                                           last-word-1-clean (subs last-word-1
                                                                   0
                                                                   (if (pos? (count last-word-1))
                                                                     (dec (count last-word-1))
                                                                     0))
                                           joined-word (str last-word-1-clean
                                                            first-word-2)]
                                       (assert (not (nil? curr-line-words)) "curr-line-words should not be nil")
                                       (assert (not (nil? last-word-1)) "last of curr-line-words should not be nil")
                                       (if (or (not (string/ends-with? last-word-1 "-"))
                                               (and (d/is-word? last-word-1-clean)
                                                    (d/is-word? first-word-2)
                                                    (not (d/is-word? joined-word))))
                                         (recur (conj result curr-line-words)
                                                next-line-words
                                                (rest remaining-lines))
                                         (let [new-words-1 (concat (butlast curr-line-words) [joined-word])
                                               new-words-2 (rest next-line-words)]
                                           (recur (conj result new-words-1)
                                                  new-words-2
                                                  (rest remaining-lines))))))))
        lines-rejoined (map (partial string/join " ") lines-split-into-words)]
    lines-rejoined))

(defn clean-parsed-text
  [text file-num]
  (let [lines (string/split-lines text)
        filtered-lines (->> (map-indexed (partial filter-line file-num) lines)
                            (keep identity))
        unhyphenated-lines (join-hyphenated-line-ends filtered-lines)]
    (string/join \newline unhyphenated-lines)))
