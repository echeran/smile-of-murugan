(ns smile-of-murugan.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [happyapi.providers.google :as google]
   [org.httpkit.encode :as enc]
   [smile-of-murugan.jm :as jm]
   [cheshire.core :as json]
   [clojure.string :as str]
   [babashka.fs :as fs]))

(happyapi.setup/prepare-config :google nil)

(defn file-path->base64
  [file-path]
  (-> file-path
      io/file
      .toPath
      java.nio.file.Files/readAllBytes
      enc/base64-encode))

(defn response-for-file-path
  "Spit the OCR output for a file at file-path."
  [file-path]
  ;; https://cloud.google.com/document-ai/docs/send-request
  (let [content (file-path->base64 file-path)] 
    (google/api-request {:method :post,
                         :url "https://us-documentai.googleapis.com/v1/projects/1074643770883/locations/us/processors/65e4acb69466373:process" 
                         :query-params {},
                         :body {"rawDocument" {"mimeType" "application/pdf",
                                               "content" content}
                                "processOptions" {"ocrConfig"
                                                  {"premiumFeatures"
                                                   {"computeStyleInfo" true}}}}
                         :scopes ["https://www.googleapis.com/auth/cloud-platform"]})))

(defn batch-response-for-file-path
  "Spit the OCR output for a file at file-path."
  [file-path]
  ;; https://cloud.google.com/document-ai/docs/send-request
  (let [content (file-path->base64 file-path)]
    (google/api-request {:method :post,
                         :url "https://us-documentai.googleapis.com/v1/projects/1074643770883/locations/us/processors/65e4acb69466373:batchProcess"
                         :query-params {},
                         :body {
                                "inputDocuments" {
                                    "gcsDocuments" {
                                      "documents" [
                                                   {
                                                    "gcsUri" "gs://echeran/Smile-of-Murugan-On-Tamil-Literature-of-South-India-Kamil-Zvelebil-Brill.pdf",
                                                    "mimeType" "application/pdf"
                                                    }
                                                   ]
                                    }
                                  }
                                "documentOutputConfig" {
                                    "gcsOutputConfig" {
                                      "gcsUri" "gs://echeran/smile-of-murugan",
                                      ;; "fieldMask" "FIELD_MASK"
                                    }
                                  },
                                }
                         :scopes ["https://www.googleapis.com/auth/cloud-platform"]})))


(defn process-response
  [docai-resp file-name]
  (let [resp-document (get docai-resp "document")
        md-lines (jm/docai-to-md resp-document)]
    (spit file-name (string/join \newline md-lines))))

(defn process-response-2
  [docai-resp file-name]
  (let [md-lines (jm/docai-to-md docai-resp)]
    (spit file-name (string/join \newline md-lines))))


(defn slurp-json
  [json-file]
  (-> (slurp json-file)
      (json/parse-string)))

(defn target-file-path
  [input-file]
  (let [basename (fs/file-name input-file)
        output-file (fs/path "outputs" basename)]
    (-> (str output-file)
        (str/replace #".json$" ".md"))))

(defn process-output
  [input-dir]
  (let [files (->> (file-seq (io/file input-dir))
                   (filter #(.isFile %)))]
    (doseq [file-path files]
      (process-response-2 (slurp-json file-path) (target-file-path file-path)))))

(defn get-number
  "Get the first number from a File or stringified File path, and return as an integer"
  [f]
  (Integer/parseInt
   (re-find #"\d+" (str f))))

(defn combine-md-files
  "Combine the markdown files into one"
  [dir output-file-name]
  (let [md (->> (jm/get-files-from-dir dir)
                (sort-by get-number)
                (map slurp)
                (str/join \newline))]
    (spit output-file-name md)))


(comment 

  ;; Sample doc - online processing - did not do

  ;; Note: only run these commands once, because OCR AI logic can change over time,
  ;; and the input scan quality is always on the border of legibility for differnet Latin diacritics
  ;; Spit the result to a file
  (def resp (response-for-file-path "sample/sample-smile.pdf"))
  (spit "sample/docai-resp.edn" (pr-str resp))

  ;; Run these commands as many times as needed, now that the response object has been persisted to a file
  (def resp (clojure.edn/read-string (slurp "sample/docai-resp.edn")))
  ;; Note: remember to detect spelling error, just like in the `inspect-low-confidence-tokens`
  ;; unit test, before running `process-response`. Detecting spelling errors
  ;; is a parallel pipeline because it needs info from the response object as well.
  ;; Detecting spelling errors and then checking the result of `process-response` is an iterative process.
  (process-response resp "sample/docai-resp-output.md")
  )

(comment

  ;; Full doc - batch processing - what we did

  ;; Note: only run these commands once, because OCR AI logic can change over time,
  ;; and the input scan quality is always on the border of legibility for differnet Latin diacritics
  ;; Spit the result to a file
  (def resp (batch-response-for-file-path "originals/Smile-of-Murugan-On-Tamil-Literature-of-South-India-Kamil-Zvelebil-Brill.pdf"))
  (spit "docai-resp.edn" (pr-str resp))

  ;; Run these commands as many times as needed, now that the response object has been persisted to a file
  (def resp (clojure.edn/read-string (slurp "docai-resp.edn")))
  ;; Note: remember to detect spelling error, just like in the `inspect-low-confidence-tokens`
  ;; unit test, before running `process-response`. Detecting spelling errors
  ;; is a parallel pipeline because it needs info from the response object as well.
  ;; Detecting spelling errors and then checking the result of `process-response` is an iterative process.
  (process-response resp "docai-resp-output.md"))


(comment

  ;; Full doc - online processing - did not do

  ;; Note: only run these commands once, because OCR AI logic can change over time,
  ;; and the input scan quality is always on the border of legibility for differnet Latin diacritics
  ;; Spit the result to a file
  (def resp (response-for-file-path "originals/Smile-of-Murugan-On-Tamil-Literature-of-South-India-Kamil-Zvelebil-Brill.pdf"))
  (spit "docai-resp.edn" (pr-str resp))

  ;; Run these commands as many times as needed, now that the response object has been persisted to a file
  (def resp (clojure.edn/read-string (slurp "docai-resp.edn")))
  ;; Note: remember to detect spelling error, just like in the `inspect-low-confidence-tokens`
  ;; unit test, before running `process-response`. Detecting spelling errors
  ;; is a parallel pipeline because it needs info from the response object as well.
  ;; Detecting spelling errors and then checking the result of `process-response` is an iterative process.
  (process-response resp "docai-resp-output.md"))




(comment
  ;; Process response - process output files from batch processing - what we did
  ;; In other words: Combine `.json` files from API into cleaned up & processed `.md` files

  (process-output "inputs"))



(comment
  ;; Combine `.md` files into one, and analyze misspellings
  (combine-md-files "outputs" "smile.md")

  (time (jm/inspect-low-confidence-tokens-from-dir "inputs" "misspellings.txt")))
