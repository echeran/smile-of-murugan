(ns smile-of-murugan.dictionary
  (:require [babashka.fs :as fs]
            [clojure.java.io :as jio])
  (:import (java.io InputStream)
           [org.apache.lucene.store NIOFSDirectory]
           [org.apache.lucene.analysis.hunspell Dictionary
                                                Hunspell]))

(def ^:private temp-dir (atom nil))

(def dict (atom nil))

(defn is-word?
  [^String s]
  (.spell @dict s))

(defn close-dictionary
  []
  (reset! dict nil)
  (when @temp-dir
    (fs/delete-if-exists @temp-dir)
    (reset! temp-dir nil)))

(defn load-dictionary
  []
  (close-dictionary)
  (reset! temp-dir (fs/create-temp-dir))
  (assert (not (nil? @temp-dir)) "temp dir should have been initialized")
  (let [^NIOFSDirectory lucene-dir (NIOFSDirectory. (fs/path @temp-dir))
        ^String temp-dir-prefix "tmp"
        ^InputStream dict-resc (jio/input-stream (jio/resource "en_US.dic"))
        ^InputStream affix-resc (jio/input-stream (jio/resource "en_US.aff"))
        hd (Dictionary. lucene-dir
                          temp-dir-prefix
                          affix-resc
                          dict-resc)
        hunspell (Hunspell. hd)]
    (reset! dict hunspell)))
