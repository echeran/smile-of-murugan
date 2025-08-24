(ns smile-of-murugan.dictionary
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as jio]
   [clojure.java.io :as io])
  (:import
   (java.io InputStream)
   [org.apache.lucene.analysis.hunspell Dictionary Hunspell]
   [org.apache.lucene.store NIOFSDirectory]))

(defonce dict (atom nil))

(defn is-word?
  [^String s]
  (some #(.spell % s) (vals @dict)))

(defn close-dictionary
  [temp-dir]
  (reset! dict nil)
  (when temp-dir
    (fs/delete-tree temp-dir)
    (fs/delete-if-exists temp-dir)))

(defn load-dictionary
  "Load the Hunspell dictionary files (.dic & .aff) for a given locale, when given
   the filename version of the locale. Ex: if the en-GB files are `en_GB.dic` and `en_GB.aff`,
   then `dict-locale-str` should be `en_GB`. This string will then become the key for the
   Hunspell object stored in the top-level `dict` map atom.
   The dictionary files should be located in the project's resources folder."
  [temp-dir dict-locale-str]
  (when-not (get @dict dict-locale-str)
    (let [^NIOFSDirectory lucene-dir (NIOFSDirectory. (fs/path temp-dir dict-locale-str))
          ^String temp-dir-prefix "tmp"
          ^InputStream dict-resc (jio/input-stream (jio/resource (str dict-locale-str ".dic")))
          ^InputStream affix-resc (jio/input-stream (jio/resource (str dict-locale-str ".aff")))
          hd (Dictionary. lucene-dir
                          temp-dir-prefix
                          affix-resc
                          dict-resc)
          hunspell (Hunspell. hd)]
      {dict-locale-str hunspell})))

(defn load-dictionaries
  [] 
  (let [;; create the temp directory that is the root of the folders in which the Hunspell Dictionary creates its Dictionary object
        temp-dir (fs/create-temp-dir)
        dict-map (apply merge [(load-dictionary temp-dir "en_GB")
                               (load-dictionary temp-dir "en_US")])]
    (reset! dict dict-map)
    (reify java.io.Closeable (close [_] (close-dictionary temp-dir)))))