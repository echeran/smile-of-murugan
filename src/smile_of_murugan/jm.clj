(ns smile-of-murugan.jm
  (:require
   [babashka.fs :as fs]
   [cheshire.core :as json]
   [clojure.string :as string]
   [clojure.string :as str]
   [smile-of-murugan.dictionary :as d]
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



(def CORRECT-SPELLINGS
  "The key is the correct spelling. The value is a set of incorrect spellings
   as they occur in the OCR text.
   If the word actually appears correctly spelled in the text, then
   the value set of strings should be kept empty."


  {;; words & names using diacritics in transliterations

   ;; Ls - ḷ ḻ
   ;; Rs - ṟ
   ;; Ns - ṉ ṇ
   ;;
   ;; NG - ṅ
   ;;
   ;; T - ṭ
   ;;
   ;; upper long vowels - Ā
   ;; lower long vowels - ā ē ī ō ū

   "absolutive" #{}
   "absolutives" #{}
   "absolutely" #{"absolutly"}
   "acai" #{}
   "acais" #{}
   "Ācān" #{"Acān"}
   "Ācāryas" #{"Acāryas"}
   "acaṭarkaḷ" #{"acaṭarkaļ"}
   "accam" #{}
   "accamē" #{"accame"}
   "Āccāṉ" #{"Accan" "Accān"}
   "accommodation" #{"accomodation"}
   "accompli" #{}
   "accu" #{}
   "āciri" #{"aciri"}
   "ācirikar" #{"acirikar"}
   "āciriyaccīr" #{"aciriyaccir"}
   "āciriyaṉ" #{"aciriyan" "aciri yan"}
   "Ancient" #{"Acient"}
   "āciriyam" #{"aciriyam"}
   "adbhuta" #{}
   ;; skip "Adigal" because we have a conflict:
   ;; "Prince Ilangô Adigal" is correct, but
   ;; "Maraimalai Adigal" should be "Maṟaimalai Adigaḷ"
   "adages" #{"adiges"}
   "Ādipurāṇa" #{"Adipurāṇa"}
   "Āditya" #{"Aditya"}
   "advaita" #{}
   "aestetic" #{"aesthetic"}
   "āgamas" #{"agamas"}
   "āgamic" #{"agamic"}
   "Ahalyā" #{"Ahalya"}
   "Ahalyā's" #{"Ahalya's"}
   "AHALYĀ" #{}
   "ahṟiṇai" #{"ahrinai" "ahriņai"}
   "ahtu" #{}
   "ai" #{}
   "aimpattoṉpatiṉmar" #{"aimpattonpatinmar"}
   "aimperuṅkāppiyam" #{"aimperunkāppiyam" "aimperuńkāppiyam"}
   "aindra" #{}
   "Ainkuṟunūṟu" #{"Ainkuruniru" "Ainkurunuru" "Ainkurunūru"}
   "ainkuṟunūṟum" #{"ainkurunurum"}
   "aintavittān" #{"aintavittän"}
   "aintiṇai" #{"aintinai" "aintiņai"}
   "aintiram" #{}
   "aintu" #{}
   "aivar" #{}
   "aiyā" #{"aiya"}
   "aiyaṉ" #{"aiyan"}
   ;; skipping "Aiyatika" because final l of "Aiyaṭikaḷ" was recognized as a non-letter punctuation
   "ajīva" #{}
   "akalam" #{}
   "akalavurai" #{}
   "akaṅkāram" #{"akankāram" "akaǹkāram"}
   "Akanāṉūṟu" #{"Akanānūru" "Akanāṇūru"}
   "Akapporuḷ" #{"Akapporul" "Akapporuļ"}
   "akappēy" #{}
   "akaram" #{}
   "akāra" #{}
   "akāram" #{}
   "Akastiyar" #{}
   "Akattiṇai" #{"Akattinai"}
   "Akattiṇaiyiyal" #{"Akattinaiyiyal"}
   "Akattiyam" #{}
   "akattiyamum" #{}
   "Akattiyan" #{}
   "Akattiyar" #{}
   "Akattiyar's" #{}
   "akattiṇai" #{"akattiņai"}
   "akaval" #{}
   "akavalar" #{}
   "akavalpā" #{}
   "akavalum" #{}
   "akavaṉ" #{"akavan"}
   "akavar" #{}
   "akavu" #{}
   "akavunar" #{}
   "Akilan" #{}
   "Akilan's" #{}
   "al" #{}
   "Alai" #{}
   "Alaikaḷ" #{"Alaikal"}
   ;; skipping "alaku" because it is both aḻaku (beauty) and alaku (bird beak)
   "Ālankuṭi" #{"Alaṅkuți"}
   "Āḷavantār" #{"Alavantār"}
   "alba" #{}
   "Allmost" #{}
   "alloform" #{}
   "Aḷḷur" #{"Allur"}
   "allākkāl" #{}
   "allō" #{}
   "aḻukai" #{"alukai"}
   "Āḻuṭaiya" #{"Alutaiya"}
   "Alvars" #{}
   "Āḻvār" #{"Alvãr" "Alvär" "Alvār"}
   "Āḻvārs" #{"Alvārs"}
   "amahat" #{}
   "Amalaṉ" #{"Amalan"}
   "amar" #{}
   "aṅkaṇam" #{"aňkaņam"}
   "artha" #{}
   "atuviṉṟeṉ" #{"atuvinren"}
   "Aruṇakiri" #{}
   "Aňciṟaittumpi" #{"Anciraittumpi"}
   "Auvaiyār" #{}
   "Ayodhyā" #{}
   "akam" #{}
   "Akam" #{}
   "Ammaiyār" #{}
   "Ammaiyār's" #{}
   "Ammā" #{}
   "Anandarangam" #{}
   "Annamalai" #{}
   "anorganic" #{}
   "anpu" #{}
   "anthologization" #{}
   "antāti" #{} 
   "Añcali" #{}
   "Añci" #{}
   "añcal" #{"aňcal"}
   "añcu" #{"aňcu"}
   "aṅkaṇam" #{"aňkaņam"} 
   "Appar" #{}
   "Appar's" #{}
   "Appu" #{}
   "argumentum" #{}
   "Aricil" #{}
   "Arikamedu" #{}
   "aṟivu" #{"arivu"}
   "Arthaśāstra" #{}
   "arukaṉ" #{"arukan"}
   "aruṇakiri" #{"arunakiri"}
   "Aruṇakiri" #{"Arunakiri" "Aruņakiri"}
   "Aruṇakiri's" #{"Arunakiri's" "Aruņakiri's"}
   "Aruṇakirinātar" #{}
   "Aruṇācala" #{}
   "Aruṇācalam" #{}
   "Arya" #{}
   "arya" #{}
   "aryas" #{}
   "Aryas" #{}
   "āsanas" #{"asanas"}
   "assonances" #{}
   "Ataṅkōṭu" #{"Atankōṭu"}
   "ataṅkōṭṭu" #{"atankōṭṭu"}
   "Ataṅkōṭṭāciriyar" #{}
   "Ataṅkōṭṭācāṉ" #{"Atankōṭṭācān" "Atankōṭṭācāṇ"}
   "Aṭikaḷ" #{"Atikal" "Ațikal" "Ațikaļ"}
   "atikāram" #{}
   "Ātittaṉ" #{"Atittan"}
   "aṭi" #{"ați"}
   "Aṭiyārkkunallār" #{}
   "Aṭiyārkkunallār's" #{}
   "Auvai" #{}
   "avai" #{}
   "avar" #{}
   "avarkalāl" #{}
   "avarkku" #{}
   "avarukku" #{}
   "avarul" #{} 

   "bhaktas" #{}
   "bhakti" #{}
   "Bharati" #{}
   "Brahmin" #{}
   "Brāhmī" #{"Brāhmi"}
   "Brahmi" #{}
   "Campantaṉ" #{"Campantan"}
   "caṅka" #{"canka"}
   "caṅkam" #{"cankam"}
   "Caṅkam" #{"Cankam" "Cańkam" "Caǹkam"}
   "CAṆKAM" #{"CANKAM" "CAŃKAM"}
   "caṅkattamiḻ" #{"cańkattamil"}
   "cāṉṟōṉ" #{"cāṇrōn" "canrōn"}
   "cāṉṟōr" #{"canrōr" "cānṛōr" "cāṇṛōr" "canyōr"}
   "cantam" #{}
   "Cenkuṭṭuvan" #{"Ceṅkuṭṭuvan"}
   "Cenkuṭṭuvan's" #{}
   "centamiḻ" #{}
   "Centamiḻ" #{"Centamil"}
   "centamiḻnaṭai" #{"centamilnațai"}
   "ceyyuḷ" #{"ceyyu" "ceyyul"}
   "choroi" #{}
   "cilampu" #{}
   "Cilappatikāram" #{"Cilappatikaram"}
   "Ciṟappatikāram" #{"Cirappatikāram"}
   "ciṟappin" #{"cirappin"}
   "ciṟappu" #{"cirappu"}
   "ciṟappuppāyiram" #{"cirappuppayiram"}
   "circumambulated" #{}
   "ciṟitu" #{"ciritu"}
   "ciṟitē" #{"ciritē"}
   "cīrtti" #{"cirtti"}
   "Ciṟupāṇ" #{"Cirupāṇ"}
   "Ciṟupāṇāṟṟuppaṭai" #{"Cirupāṇārruppatai" "Cirupāṇārruppaṭai"}
   "Citamparaṉār" #{"Citamparaṇār"}
   "cittan" #{}
   "cittar" #{}
   "Cittar" #{}
   "CITTAR" #{}
   "cittavaittiyam" #{}
   "Cīvakacintāmaṇi" #{"Civakacintāmaṇi" "Civakacintamani"}
   "civam" #{}
   "civan" #{}
   "Civaperumāṉ" #{"Civaperumān"}
   "Civapurāṇam" #{"Civapuraṇam"}
   "Civaväkkiyar" #{"Civavākkiyar"}
   "civayam" #{"civayam"}
   "Civañāṉa" #{"Civaňāṇa" "Civañāṇa"}
   "civāyam" #{}
   "Collatikāram" #{}
   "commentatorial" #{}
   "contraire" #{}
   "coṟporuḷ" #{"corporul"}
   "Cuntaraṉ" #{"Cuntaran"}
   "Cuntarar" #{}
   "Cuntarar's" #{}
   "Cuvāmi" #{}
   "cāl" #{}
   "cālpu" #{}
   "Cēral" #{}
   "cuttiram" #{}
   "democratism" #{}
   "dramatis" #{}
   "Eḻuttāḷaṉ" #{"Eluttālan"}
   "explicitly" #{"explicitely"} ;; the rare instance of a misspelled English word!
   "foetus" #{}
   "Gaṇapati" #{}
   "gloire" #{}
   "glossators" #{}
   "gray" #{}
   "il" #{}
   "Iḷampūraṇar" #{"Ilampūraṇar"}
   "Jaina" #{}
   "Kailasapathy" #{}
   "Kailasapathy's" #{}
   "Kampaṉ's" #{"Kampan's"}
   "kiḻār" #{"kilār"}
   "kāňci" #{}
   "kaṭaṉ" #{"katan"}
   "kēḷ" #{"kēļ"}
   "kiḷavi" #{"kilavi"}
   "Kiḷimaṅkalaṅ-" #{"Kilimankalan-"}
   "Kiḷimaṅkalaṅkiḻār" #{"Kilimankalankilār"}
   "kiḷa" #{"kiļa"}
   "kiṇai" #{"kiņai"}
   "koṭukkuvar" #{"koṭukkuvar"}
   "Kunstdichtung" #{}
   "kuruṭarkaḷ" #{"kuruṭarkaļ"}
   "kūttar" #{"kuttar"}
   "kūtti" #{"kutti"}
   "Kuṟaḷ" #{"Kuṛaļ"}
   "Kuṭṭuvaṉ" #{"Kuṭṭuvan"}
   "kāma" #{}
   "kēḷir" #{"kēļir"}
   "kēḷvi" #{"kēļvi"}
   "Languedoc" #{}
   "liṅka" #{"linka"}
   "liveable" #{"livable"} ;; seems like another English misspelling, right? 
   "makaḷir" #{"makalir"}
   "maṉai" #{"manai"}
   "maṇpukku" #{"manpukku"}
   "mantic" #{"mantic"}
   "Maturai" #{}
   "monistic" #{}
   "Montaignesque" #{}
   "Murukaṉ" #{"Murukan"}
   "Mutukoṟ-" #{"Mutukor-"}
   "Mutukoṟ" #{"Mutukor"}
   "Māmūlaṉār" #{"Māmūlaṇār"}
   "māyvatu" #{}
   "moralizations" #{}
   "Muciṟi" #{"Muciri"}
   "Nampi" #{}
   "Nandivarman" #{}
   "Nantikkalampakam" #{}
   "nāgarika" #{}
   "nāṇ" #{}
   "niṟīit" #{"nirīit"}
   "paṭṭuṉ" #{"paṭṭun"}
   "Pallava" #{}
   "paṇpuṭaiyār" #{"panpuṭaiyar"}
   "Parimēlaḻakar" #{"Parimēlaļakar"}
   "paṇ" #{}
   "perumpeyar" #{}
   "politikon" #{}
   "porunar" #{}
   "pre" #{}
   "pukaḻ" #{"pukal"}
   "pukaḻeṉiṉ" #{"pukalenin"}
   "Pukaḻēnti" #{"Pukaļēnti"}
   "pukaḻnta" #{"pukalnta"}
   "pulaitti" #{}
   "pulavar" #{}
   "puṟam" #{}
   "Puṟam" #{"Puram"}
   "Puṟapporuḷ" #{"Purapporul"}
   "Putumaippittaṉ" #{"Putumaippittan"}
   "pāṇ" #{}
   "pāṇar" #{"paṇar"}
   "pāṇi" #{}
   "Pārati" #{}
   "reflexion" #{}
   "religio" #{}
   "Sangam" #{}
   "Sanskritic" #{}
   "Sanskritized" #{}
   "scholiasts" #{}
   "superstratum" #{}
   "tampukaḷ" #{"tampukaļ"}
   "taṭāri" #{}
   "Tirukkuṟaḷ" #{"Tirukkural" "Tirukkuraļ"}
   "Tolkāppiyam" #{}
   "trobadors" #{}
   "tāmmāyntaṉar" #{"tāmmāyntaṇar"}
   "tulakam" #{}
   "Tulu" #{}
   "unimpassioned" #{}
   "urai" #{}
   "Uṟaiyūr" #{"Uraiyur" "Uraiyūr"}
   "uyiruṅ" #{"uyiruń"}
   "Vedic" #{}
   "Venkataswamy" #{}
   "veṇpāmālai" #{"veņpāmālai"}
   "viḻuppuṇ" #{"viluppun"}
   "viṟaliyar" #{"viraliyar"}
   "Volksliteratur" #{}
   "vākai" #{}
   "worldly" #{"wordly"}
   "yāḻ" #{"yal"}
   "yātum" #{}
   "yāvaruṅ" #{"yāvarun"}
   "zoon" #{}
   "élite" #{}
   "āṭunar" #{}
   "ūrē" #{}
   ;; abbreviations & names 
   "A.C." #{}
   "A.D" #{}
   "A.D." #{}
   "A.K." #{}
   "Acknowl" #{"Acknowl"}
   "acutangula" #{}
   "Agastya" #{}
   "Agastya's" #{}
   "Agravala" #{}
   "Aink" #{}
   "Aiṅk" #{"Aiñk" "Aiňk"}
   "Aiyangar" #{}
   "Aiyangar's" #{}
   "Aiyaṅkār" #{"Aiyankār"}
   "Aiyaṅkār's" #{"Aiyankār's"}
   "Aiyaṉār" #{"Aiyaṇār"}
   "Aiyar" #{}
   "Aiyar's" #{}
   "Aiyengar" #{}
   "Ak" #{}
   "Akademi" #{"Akadami"}
   "Akapp" #{}
   "Akat" #{}
   "Akatt" #{}
   "Alagirisami" #{}
   "Alagiriswamy" #{}
   "Alain" #{}
   "Albertine" #{}
   "Albin" #{}
   "Amaiyappa" #{}
   "Arangasami" #{}
   "Aravintan" #{}
   "Asher" #{}
   "Asiatique" #{}
   "Ayyangar" #{}
   "Ayyankār" #{}
   "Ayyappan" #{}
   "Ayyar" #{}
   "B.C." #{}
   "Babington" #{}
   "Balakrishna" #{}
   "Balakrishnan" #{}
   "Balasubramaniam" #{}
   "Barringtonia" #{}
   "Beschi" #{}
   "Beschi's" #{}
   "CAMARPPAṆAM" #{"CAMARPPAŅAM"}
   "cca" #{}
   "Campantar" #{}
   "Chellappa" #{}
   "Chellappa's" #{}
   "Chelliah" #{}
   "Chera" #{}
   "Cheranad" #{}
   "Cheras" #{}
   "Chettiar" #{}
   "Chidambara" #{}
   "Chidambaram" #{}
   "Chidambaranatha" #{}
   "Chokkalingam" #{}
   "Chola" #{}
   "Cholas" #{}
   "BSOAS" #{}
   "DBIA" #{}
   "DED" #{}
   "Jaffna" #{}
   "Jains" #{}
   "Janakiraman" #{}
   "Jesudasan" #{}
   "Jeyakanthan" #{}
   "Kur" #{}
   "Ramaswamy" #{}
   "e.g." #{}
   "i.e." #{}
   "lw" #{}
   "Skt" #{}
   ;; puncutation
   "”" #{}
   "“" #{}
   "—" #{}
   ;; REMOVE
   ;;   (relies on the fact that number of repeated whitespace is not significant in Markdown)
   ;;   Note: we have the 'remove' category because of a human marking in the margin that got interpreted as a "×"
   "" #{"×"}})

(def SPELLING-CORRECTIONS
  (into {} (for [[k vs] CORRECT-SPELLINGS
                 v vs]
             [v k])))

"cāṉṟōr" ;; Elango edition - NFD form
"cāṉṟōr" ;; Internet inspired - NFC form

(defn keep-misspelled-non-english-words
  [word-index-scores]
  (let [trimmed-words (map #(update-in % [:substring] string/trim) word-index-scores)
        no-punctuation (remove #(re-matches #"[\p{Space}\p{Punct}]+" (:substring %)) trimmed-words)
        no-english (remove #(d/is-word? (:substring %)) no-punctuation)
        no-correct-non-english (remove #(CORRECT-SPELLINGS (:substring %)) no-english)
        no-visited-incorrect-non-english (remove #(SPELLING-CORRECTIONS (:substring %)) no-correct-non-english)]
    ;; TODO: remove all words of a high confidence score that match the English dictionary
    no-visited-incorrect-non-english))

(defn format-word-index-score-output
  [word-index-score]
  (str (pr-str (:substring word-index-score))
       \tab
       (pr-str (:context word-index-score))))

(defn inspect-low-confidence-tokens
  "Take a response JSON body document from the Doc AI API, analyze word confidence,
   and print the lowest confidence words (confidence in OCR output/quality) to a text file"
  [in-file-seq out-file]
  (with-open [_ (d/load-dictionaries)]
    (spit out-file (->> (for [in-file in-file-seq]
                          (let [json-file (fs/file in-file)
                                json-str (slurp json-file)
                                resp (json/parse-string json-str)
                                word-index-scores (get-word-index-scores resp)
                                ordered-word-index-scores (->> word-index-scores
                                                               (keep-misspelled-non-english-words))]
                            ordered-word-index-scores))
                        (mapcat identity)
                        (sort-by (comp str/lower-case :substring))
                        (map format-word-index-score-output)
                        (string/join \newline)))))

(defn get-files-from-dir
  [dir]
  (->> (fs/file dir)
       (file-seq)
       (filter #(.isFile %))))

(defn inspect-low-confidence-tokens-from-dir
  [dir out-file]
  (let [files (get-files-from-dir dir)]
    (inspect-low-confidence-tokens files out-file)))


(def TOKEN-MARKDOWN-SYNTAX-MATCHER
  #"([\*_]*)(.*?)([\*_\p{Punct}]*)")

(defn correct-spelling-in-token
  [token]
  (let [groups (re-matches TOKEN-MARKDOWN-SYNTAX-MATCHER token)
        [_ leading word trailing] groups
        word-corrected-spelling (get SPELLING-CORRECTIONS word word)]
    (str leading word-corrected-spelling trailing)))

(defn correct-spellings-on-line
  [line]
  (let [tokens (string/split line #"\s+")
        tokens-corrected-spelling (map correct-spelling-in-token tokens)]
    (string/join " " tokens-corrected-spelling)))

(defn docai-to-md
  [resp]
  (let [text (get-stylized-text resp)
        lines (string/split-lines text)
        unhyphenated-lines (transform/join-hyphenated-line-ends lines)
        lines-with-paragraphs (insert-paragraph-lines unhyphenated-lines)
        lines-with-paragraphs-corrected-spelling (map correct-spellings-on-line lines-with-paragraphs)]
    lines-with-paragraphs-corrected-spelling))

(defn docai-json-to-md
  [resp-json]
  (let [resp (json/parse-string resp-json)]
    (docai-to-md resp)))
