(ns smile-of-murugan.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [happyapi.providers.google :as google]
   [org.httpkit.encode :as enc]
   [smile-of-murugan.jm :as jm]))

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

(comment
  ;; Note: only run this once, because OCR AI logic can change over time,
  ;; and the input scan quality is always on the border of legibility for differnet Latin diacritics
  ;; Spit the result to a file
  (def resp (response-for-file-path "sample/sample-smile.pdf"))
  )

(defn process-response
  [docai-resp]
  (let [resp-document (get docai-resp "document")
        md-lines (jm/docai-to-md resp-document)]
    (spit "docai-resp-output.md" (string/join \newline md-lines))))

(comment 
  (def resp (clojure.edn/read-string (slurp "docai-resp.edn")))
  ;; Note: remember to detect spelling error, just like in the `inspect-low-confidence-tokens`
  ;; unit test, before running `process-response`. Detecting spelling errors
  ;; is a parallel pipeline because it needs info from the response object as well.
  ;; Detecting spelling errors and then checking the result of `process-response` is an iterative process.
  (process-response resp)
  )
