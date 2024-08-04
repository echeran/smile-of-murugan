(ns smile-of-murugan.core
  (:require [happyapi.providers.google :as google]
            [happyapi.google.youtube-v3 :as youtube]))

(defn f
  []
  (let [resp (youtube/channels-list "contentDetails,statistics" {:forUsername "ClojureTV"})]
    resp))

(defn f2
  []
  )