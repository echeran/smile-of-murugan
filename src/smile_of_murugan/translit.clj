(ns smile-of-murugan.translit
  (:require [clojure.set :as set]
            [clj-thamil.format :as fmt]
            [clj-thamil.format.convert :as cvt]))

(def ^{:doc "map of தமிழ்->ISO 15919 (latin script) transliteration"}
  தமிழ்-iso15919-phoneme-map
  {"அ" "a"
   "ஆ" "ā"
   "இ" "i"
   "ஈ" "ī"
   "உ" "u"
   "ஊ" "ū"
   "எ" "e"
   "ஏ" "ē"
   "ஐ" "ai"
   "ஒ" "o"
   "ஓ" "ō"
   "ஔ" "au"
   "க்" "k"
   "ங்" "ṅ"
   "ச்" "c"
   "ஞ்" "ň"
   "ட்" "ṭ"
   "ண்" "ṇ"
   "த்" "t"
   "ந்" "n"
   "ப்" "p"
   "ம்" "m"
   "ய்" "y"
   "ர்" "r"
   "ல்" "l"
   "வ்" "v"
   "ழ்" "ḻ"
   "ள்" "ḷ"
   "ற்" "ṟ"
   "ன்" "ṉ"})

(def ^{:doc "an inverse of தமிழ்->iso15919-phoneme-map"}
  iso15919-தமிழ்-phoneme-map
  (set/map-invert தமிழ்-iso15919-phoneme-map))


(def iso15919-தமிழ்-phoneme-trie (fmt/make-trie iso15919-தமிழ்-phoneme-map))

(def தமிழ்-iso15919-phoneme-trie (fmt/make-trie தமிழ்-iso15919-phoneme-map))

(defn iso15919->தமிழ்
  "transliterates a string of Latin script (ISO 15919 transliterated தமிழ்) into the தமிழ் that it represents"
  [s]
  (fmt/phonemes->str (fmt/str->elems iso15919-தமிழ்-phoneme-trie s)))

(defn தமிழ்->iso15919
  "transliterates a தமிழ் string into Latin script (ISO 15919 transliterated தமிழ்)"
  [s]
  (->> (fmt/str->phonemes s)
       (apply str)
       (fmt/str->elems தமிழ்-iso15919-phoneme-trie)
       (apply str)))