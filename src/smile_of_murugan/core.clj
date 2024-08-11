(ns smile-of-murugan.core
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [happyapi.google.documentai-v1 :as docai]
   [happyapi.google.vision-v1 :as gimg]
   [happyapi.google.youtube-v3 :as youtube]
   [org.httpkit.encode :as enc]
   [smile-of-murugan.dictionary :as d]
   [smile-of-murugan.transform :as t]))

(defn f
  []
  (let [resp (youtube/channels-list "contentDetails,statistics" {:forUsername "ClojureTV"})]
    resp))

(defn f3 []
  (let [x (string/split)]))

(defn f2
  []
  (let [;; location (docai/projects-locations-list)
        processor (docai/projects-locations-processors-create)]))

(defn file-path->base64
  [file-path]
  (-> file-path
      io/file
      .toPath
      java.nio.file.Files/readAllBytes
      enc/base64-encode))

(defn resp->full-text
  [resp]
  (let [full-text-annotation (-> resp
                                 :fullTextAnnotation)
        single-page-details (-> full-text-annotation :pages first)
        confidence (:confidence single-page-details)]
    (when (some-> confidence
                  (> 0.8))
      (:text full-text-annotation))))

(defn response-for-file-path
  "Spit the OCR output for a file at file-path."
  [file-path]
  ;;(credentials/init!)
  (gimg/images-annotate {:requests [{:image        {:content (file-path->base64 file-path)
                                              ;;:source {:imageUri "https://i0.wp.com/static.flickr.com/102/308775600_4ca34de425_o.jpg"}
                                                    },
                                     :features     [{:type "DOCUMENT_TEXT_DETECTION"}],
                                     :imageContext {:languageHints ["Tamil" "English"]}}]}))

(defn extract-text-for-file-path
  "Spit the OCR output for a file at file-path."
  [file-path]
  ;;(credentials/init!)
  (let [query-response (response-for-file-path file-path)
        full-text (-> query-response
                      :responses
                      first
                      resp->full-text)]
    full-text))

(defn spit-text-to-file
  [text in-file-path]
  (let [out-file-path (str in-file-path ".txt")]
    (spit out-file-path text)))

(defn find-all-file-paths
  [dir]
  (->> (io/file dir)
       file-seq
       (filter #(.isFile %))))

(defn setup
  []
  (d/load-dictionary))

(defn teardown
  []
  (d/close-dictionary))

(defn convert-images
  [dir]
  (setup)
  (let [file-paths (->> (find-all-file-paths dir)
                        (filter #(string/ends-with? (str %) ".png")))
        sorted-paths (->> (sort file-paths))]
    (doseq [[idx path] (map-indexed vector sorted-paths)]
      (some-> path
              extract-text-for-file-path
              (t/clean-parsed-text (inc idx))
              (spit-text-to-file path))))
  (teardown))

(defn chapter-id
  [page-num]
  (let [start-num->name [[1 :ch1]
                         [10 :ch2]
                         [20 :ch3]
                         [30 :ch4]]]
    (->> start-num->name
         (take-while #(<= (first %) page-num))
         last
         second)))

(defn combine-chapters
  [in-dir out-dir]
  (setup)
  (let [file-paths (->> (find-all-file-paths in-dir)
                        (filter #(string/ends-with? (str %) ".txt")))
        sorted-paths (sort file-paths)]
    (letfn [(concat-file [chapters-map path]
              (let [file (fs/file path)
                    file-name (.getName file)
                    file-num-str (-> (re-seq #"(\d+).txt" file-name)
                                     first
                                     (nth 1))
                    file-num (Integer/parseInt file-num-str)
                    chapter (chapter-id file-num)
                    text (slurp file)]
                (update-in chapters-map [chapter] str \newline text)))]
      (let [combined-chapters-map (-> (reduce concat-file {} sorted-paths)
                                      (dissoc nil))]
        (doseq [[chapter text] combined-chapters-map]
          (let [text-lines (string/split-lines text)
                unhyphenated-lines (t/join-hyphenated-line-ends text-lines)
                chapter-file-name (str (name chapter) ".txt")
                chapter-file (fs/file out-dir chapter-file-name)]
            (spit chapter-file (string/join \newline unhyphenated-lines)))))))
  (teardown))