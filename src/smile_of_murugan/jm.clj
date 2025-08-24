(ns smile-of-murugan.jm
  (:require
   [babashka.fs :as fs]
   [cheshire.core :as json]
   [clojure.string :as string]
   [clojure.string :as str]
   [smile-of-murugan.dictionary :as d]
   [smile-of-murugan.transform :as transform]
   [smile-of-murugan.translit :as translit]))

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

(defn get-stylized-text
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
                               :transliteration  (-> substring
                                                     string/trim
                                                     string/lower-case
                                                     translit/iso15919->தமிழ்)
                               :context (subs text
                                              (max 0 (- start-index context-length))
                                              (min text-length (+ end-index context-length)))}))]
    word-index-scores))



(def CORRECT-SPELLINGS
  "The key is the correct spelling. The value is a set of incorrect spellings
   as they occur in the OCR text.
   If the word actually appears correctly spelled in the text, then
   the value set of strings should be kept empty."


  ;; Known bad diacritics to look for once the spelling fixes are done:
  ;;
  ;; ń ǹ ļ ț ṛ ä ţ
  ;;
  

  {;; words & names using diacritics in transliterations
   
   ;; Ls - ḷ ḻ
   ;; Rs - ṟ
   ;; Ns - ṉ ṇ
   ;;
   ;; NG - ṅ
   ;;
   ;; NY - ň 
   ;;
   ;; T - ṭ
   ;;
   ;; SH - ṣ
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
   "Akapporuḷ" #{"Akapporul" "Akapporuļ" "Akapporu!"}
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
   "Āḻvār" #{"Alvãr" "Alvär" "Alvār" "Ālvār" "Āļvār"}
   "Āḻvārs" #{"Alvārs" "Āļvārs"}
   "amahat" #{}
   "Amalaṉ" #{"Amalan"}
   "amar" #{}
   "aṅkaṇam" #{"aňkaņam"}
   "ஆரல்" #{}
   "artha" #{}
   "atuviṉṟeṉ" #{"atuvinren"}
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
   [:t "அண்டாள்"] #{"Aṇṭāl"}
   [:t "அண்டார்"] #{}
   "Appar" #{}
   "Appar's" #{}
   "Appu" #{}
   "அறம்" #{"aṛam"}
   "ஆராய்ச்சி" #{}
   [:t "ஆராய்ச்சி"] #{}
   "argumentum" #{}
   "Aricil" #{}
   "Arikamedu" #{}
   "aṟivu" #{"arivu"}
   "Arthaśāstra" #{}
   "arukaṉ" #{"arukan"}
   [:t "ஆறுமுக"] #{"Ārumuka"}
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
   "Aśoka" #{}
   "Aśoka's" #{}
   "assonances" #{}
   "Ataṅkōṭu" #{"Atankōṭu"}
   "ataṅkōṭṭu" #{"atankōṭṭu"}
   "Ataṅkōṭṭāciriyar" #{}
   "Ataṅkōṭṭācāṉ" #{"Atankōṭṭācān" "Atankōṭṭācāṇ"}
   "Aṭikaḷ" #{"Atikal" "Ațikal" "Ațikaļ" "Aṭikaļ"}
   "atikāram" #{}
   "Ātittaṉ" #{"Atittan"}
   "aṭi" #{"ați"}
   "அடியார்" #{"ațiyar" "ațiyār"}
   "அடியான்" #{"ațiyān"}
   "Aṭiyārkkunallār" #{}
   "Aṭiyārkkunallār's" #{}
   "Auvai" #{}
   "avai" #{}
   "avar" #{}
   "avarkalāl" #{}
   "avarkku" #{}
   "avarukku" #{}
   "avarul" #{}
   "அய்தம்" #{}
   "Ayurvedic" #{}
   "Bhāgavatapurāṇa" #{}
   "bhaktas" #{}
   "bhakti" #{}
   "bhakta" #{}
   "Bhakti" #{}
   "BHAKTI" #{}
   "Bharata" #{}
   "Bharata's" #{}
   "Bharathi" #{}
   "Bharathi's" #{}
   "Bharati" #{}
   "Bharati's" #{}
   "Bharatidasan" #{}
   "Bharatiya" #{}
   "Bharatiyar" #{}
   "bhāṣāsamskytayogam" #{}
   "Brahmin" #{}
   "Brāhmī" #{"Brāhmi"}
   "Brahmi" #{}
   "Campantaṉ" #{"Campantan"}
   "caṅka" #{"canka"}
   "caṅkam" #{"cankam"}
   "Caṅkam" #{"Cankam" "Cańkam" "Caǹkam"}
   [:u "சங்கம்"] #{"CANKAM" "CAŃKAM"}
   "caṅkattamiḻ" #{"cańkattamil"}
   "cāṉṟōṉ" #{"cāṇrōn" "canrōn"}
   "cāṉṟōr" #{"canrōr" "cānṛōr" "cāṇṛōr" "canyōr"}
   "cantam" #{}
   "Carittirak" #{}
   "carittiram" #{}
   "Carittiram" #{}
   "Cenkuṭṭuvan" #{"Ceṅkuṭṭuvan"}
   "Cenkuṭṭuvan's" #{}
   "centamiḻ" #{"centamil"}
   "Centamiḻ" #{"Centamil"}
   "centamiḻnaṭai" #{"centamilnațai"}
   "செந்தொடை" #{}
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
   "Cīvakacintāmaṇi" #{"Civakacintāmaṇi" "Civakacintamani" "Cīvakacintamani" "Cīvakacintāmaņi"}
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
   "Cāminātaiyar" #{"Cāmiņātaiyar"}
   "Cēkkiḻār" #{"Cēkkilär" "Cēkkiļār"}
   "Cēral" #{}
   "Cēralātaṉ" #{"Cēralātan" "Cēralātaṇ"}
   "Cēramāṉ" #{"Cēramān" "Cēramāṇ"}
   "cēri" #{}
   "Cēṉāvaraiyar" #{"Cēṇāvaraiyar"}
   "Cēṉāvaraiyar's" #{"Cēṇāvaraiyar's"}
   "Cōḻa" #{"Cōla"}
   "Cōḻaṉ" #{"Cōlan"}
   "Cōḻanāṭu" #{"Cōlanāțu" "Cōlanǎțu"}
   "சுடர்" #{"cuțar"}
   "cuttiram" #{}
   "democratism" #{}
   "dramatis" #{}
   "Durgā" #{"Durga"}
   "Duryodhana" #{}
   "Eḻuttāḷaṉ" #{"Eluttālan"}
   "Eḻuttatikāram" #{"Eluttatikarām" "Eluttatikāram"}
   "eḻuttatikāram" #{"eluttatikāram"}
   "eḻuttu" #{"eluttu"}
   "Eḻuttu" #{"Eluttu"}
   "eḻutukiṟēṉ" #{"elutukiren"}
   "eṉpa" #{"enpa"}
   "eṇperuttokai" #{"enperuttokai"}
   "epigraphic" #{}
   "epos" #{}
   "eruditory" #{}
   "et" #{}
   "Eṭṭuttokai" #{"Ettuttokai"}
   "etukai" #{}
   "Etukai" #{}
   "expliciteness" #{"expliciteness"}  ;; the rare instance of a misspelled English word!
   "explicitly" #{"explicitely"} ;; the rare instance of a misspelled English word!
   "exposé" #{}
   "foetus" #{}
   "Gajabāhu" #{"Gajabahu" "Gajabähu"}
   "Gaṇapati" #{"Ganapati"}
   "gandharva" #{}
   "Gaṅgā" #{"Gangā" "Ganga"}
   "Gaṇeṣa" #{"Gaņeṣa"}
   "Gaṇeśa" #{}
   "Geschichte" #{}
   "gloire" #{}
   "glossators" #{}
   "gray" #{}
   "hagiographic" #{}
   "icai" #{}
   "il" #{}
   "ilakkaṇam" #{"ilakkanam" "ilakkaņam"}
   "Ilakkiya" #{}
   "ilakkiyam" #{}
   "Ilakkiyam" #{}
   "Ilangô" #{}
   "Iḷaṅkōvaṭikaḷ" #{"Ilankovațikaļ" "Ilankōvaṭikaļ" "Iļankōvațikal" "Iļankovațikaļ" "Iļankōvațikaļ"}
   "Iḷaṅkōvaṭikaḷ's" #{"Ilankovaṭikal's" "Ilankōvațikal's"}
   "Iḷaṅkō" #{"Ilankō" "Iļankō"}
   "Iḷaṅkō's" #{"Ilankō's"}
   [:t "இலந்திரையம்"] #{"Ilantiraiyam"}
   [:t "இலந்திரையன்"] #{"Ilantiraiyan"}
   "Imayavarampaṉ" #{"Imayavarampan" "Imayavarampaŋ" "Imayavarampaṇ"}
   "Indianness" #{}
   "Indo" #{}
   "Indragopa" #{}
   "Indragopa's" #{}
   "Indu" #{}
   "Indu's" #{}
   "Inscriptional" #{}
   "inscriptional" #{}
   "intellection" #{}
   "invocatory" #{}
   "iṟaicci" #{"iraicci"}
   "Iṟaiyaṉār" #{"Iraiyanar" "Iraiyanār" "Iraiyaṇār"}
   "Iṟaiyaṉār's" #{"Iraiyaṇār's"}
   "Irumpoṟai" #{"Irumporai"}
   "iruntu" #{}
   "iruttal" #{}
   "Irākava" #{}
   "Irāmāvatāram" #{}
   "Irāvaṇaṉ" #{"Irāvaṇan" "Irāvaṇaṇ"}
   "iyal" #{}
   "iyaṟcīr" #{"iyarcir" "iyaṛcir"}
   "Iḷampūraṇar" #{"Iļampūraṇar" "Ilampūraṇar"}
   "Iḷampūraṇar's" #{"Iļampūraṇar's"}
   "iṭaiccankam" #{}
   "Iṭaiccankam" #{}
   "Jaina" #{}
   "Kailasapathy" #{}
   "Kailasapathy's" #{}
   [:t "காக்கைபாடினியார்"] #{"Kākkaippāṭiniyār" "Kākkaipāṭiniyār" "Kākkaipāṭiṇiyār"}
   "காலம்" #{}
   "Kaḷaviyal" #{"Kalaviyal"}
   "kaḷavu" #{"kalavu" "kaļavu"}
   "kali" #{}
   "Kalittokai" #{}
   "Kalki" #{}
   "Kalki's" #{}
   "கைக்கிளை" #{"kaikkilai" "kaikkiļai"}
   [:t "கலிங்கத்துப்பரணி"] #{"Kalinkattupparani" "Kalinkattupparaṇi"}
   "kallā" #{}
   "Kallāṭar" #{}
   "Kaliyāṇacuntara" #{}
   "கள்வன்" #{"kaļvan"}
   "Kalyāṇacuntara" #{}
   "Kalyāṇacuntaram" #{}
   [:t "காம"] #{"Kāma"}
   [:t "கமலாம்பாள்"] #{"Kamalāmpāl" "Kamalāmpāļ"}
   "காமம்" #{}
   [:t "காமத்துப்பால்"] #{}
   "Kampaṉ" #{"Kampan"}
   "Kampaṉ's" #{"Kampan's"}
   [:t "கணக்காயனார்"] #{"Kaṇakkāyaṇār"}
   "கணக்காயனார்" #{"kaṇakkāyaṇār"}
   "காஞ்சி" #{"kāñci"}
   [:t "காஞ்சி"] #{"Kāñci" "Kāňcī"}
   [:t "காஞ்சிபுரம்"] #{"Kāñcipuram" "Kāñcīpuram" "Kāňcipuram" "Kāňcīpuram"}
   [:t "கண்ணகி"] #{"Kannaki" "Kaņṇaki" "Kanṇaki"}
   [:t "கண்ணகி's"] #{"Kaņṇaki's"}
   [:t "கண்ணனார்"] #{"Kaṇṇaṇār"}
   [:t "கண்ணப்பர்"] #{"Kaņṇappar"}
   [:t "கண்ணதாசன்"] #{"Kaṇṇatācan"}
   "காந்தள்" #{"kāntal" "kāntaļ"}
   [:t "கபாடபுரம்"] #{}
   "கபிலர்" #{}
   [:t "கபிலர்"] #{}
   [:t "காரைக்கால்"] #{}
   [:t "கரிகால்"] #{}
   [:t "கரிகாலன்"] #{"Karikālaṇ"}
   "கற்பு" #{"karpu"}
   [:t "கற்பு"] #{"Karpu"}
   "karu" #{}
   "Karu" #{}
   "கருப்பொருள்" #{"karupporu" "karupporul"}
   "karuttu" #{}
   "karutturai" #{}
   "katai"	#{}
   "Katai"	#{}
   [:t "கடைச்சங்கம்"] #{"Kaṭaiccankam"}
   "கடைச்சங்கம்" #{"kaṭaiccankam" "kaṭaiccańkam"}
   "காதல்"	#{"katal"}
   "கடவுளும்" #{"kaṭavulum"}
   "கடவுள்" #{"kaṭavuļ"}
   "கடவுள்வாழ்த்து" #{"kaṭavuļvāļttu"}
   "காட்சி" #{}
   [:t "காட்டுவாத்து"] #{"Kāṭṭuvattu"}
   "Kauṭilya" #{"Kautilya"}
   "Kauṭilya's" #{"Kauṭilya's" "Kautilya's"}
   "காவல்" #{}
   [:t "காவல்"] #{}
   "kavi" #{}
   "Kavi" #{}
   "Kavirājamārga" #{}
   "Kavirāyar" #{}
   [:t "காவிரி"] #{}
   "kavitai" #{}
   "Kavunti" #{}
   "Kayavāku" #{}
   "kayavāku" #{}
   "kiḻār" #{"kilār"}
   "kāňci" #{}
   "kaṭaṉ" #{"katan"}
   "kēḷ" #{"kēļ"}
   "Kerala" #{}
   [:t "கிழார்"] #{"Kilār" "Kiļār"}
   [:t "கிழான்"] #{"Kilān" "Kilāņ"}
   "kiḷavi" #{"kilavi"}
   "Kiḷimaṅkalaṅ-" #{"Kilimankalan-"}
   "Kiḷimaṅkalaṅkiḻār" #{"Kilimankalankilār"}
   "kiḷa" #{"kiļa"}
   "Kilpakkam" #{}
   "Kilpauk" #{}
   "kiṇai" #{"kiņai"}
   "கீர்த்தனை" #{"kirttanai"}
   "Kokilam" #{}
   "கொக்கு" #{}
   "கொல்லாமை" #{}
   "Kongu" #{}
   "கொங்கு" #{"Konku"}
   "கொற்கை" #{"Korkai"}
   [:t "கொர்ரவை"] #{"Korravai"}
   "கோவை" #{}
   "Kovalan" #{}
   [:t "கோவலன்"] #{"Kōvalan" "Kōvalaṇ"}
   [:t "கோவலன்'s"] #{"Kōvalan's"}
   "koṭukkuvar" #{"koṭukkuvar"}
   "கோயில்" #{}
   [:t "கோயில்"] #{}
   "Krishnaswami" #{}
   "Krishnaswamy" #{}
   "Kṛṣṇa" #{}
   "Kṛṣṇa's" #{}
   "Kuala" #{}
   "Kuiper" #{}
   "குலசேகர" #{}
   [:t "குழந்தை"] #{"Kulantai"}
   [:t "குலோத்துங்க"] #{"Kulōttunka"}
   [:t "குலோத்துங்கன்"] #{"Kulōttunkan"}
   [:t "குமரி"] #{}
   [:t "குமட்டூர்க்"] #{}
   "Kunstdichtung" #{}
   [:t "குறள்"] #{"Kural" "Kuraļ" "Kurral" "Kurraļ" "Kuṛaļ"}
   "குரவை" #{}
   [:t "குறிஞ்சிப்பாட்டு"] #{"Kurincippaṭṭu" "Kurincippāṭṭu"}
   "குறிப்பு" #{"kurippu"}
   "குறிப்புரை" #{"kurippurai"}
   "குறிஞ்சி" #{"kuriñci" "kuriňci" "kuriйci"}
   [:t "குறுந்தொகை"] #{"Kuruntokai"}
   "குறுந்தொகை" #{"kuruntokai"}
   "kuruṭarkaḷ" #{"kuruṭarkaļ"}
   [:t "குடி"] #{"Kuți"}
   "குடி" #{"kuți"}
   "kūttar" #{"kuttar"}
   "kūtti" #{"kutti"}
   "Kuṭṭuvaṉ" #{"Kuṭṭuvan"}
   "குவளை" #{"kuvalai" "kuvaļai"}
   "குயில்" #{}
   [:t "குயில்"] #{}
   "kāma" #{}
   "kēḷir" #{"kēļir"}
   "kēḷvi" #{"kēļvi"}
   "Lakṣmaṇa" #{}
   "Languedoc" #{}
   [:t "லங்கா"] #{"Lankā"}
   "Lesky" #{}
   "lettres" #{}
   "L'Inde" #{}
   "l'Inde" #{}
   "lingam" #{}
   "liṅka" #{"linka"}
   "Literatur" #{}
   "Literaturen" #{}
   "literatures" #{}
   "liveable" #{"livable"} ;; seems like another English misspelling, right? 
   "Lumpur" #{}
   "Mahadevan" #{}
   "Mahendravarman" #{}
   "Mahfil" #{}
   "Mahābhārata" #{}
   "Mahābhāṣya" #{}
   "Mahāvaṃso" #{"Mahāvamso"}
   "makaḷir" #{"makalir"}
   "மலை" #{}
   [:t "மலை"] #{}
   "மாலை" #{}
   [:t "மலைபடுகடாம்"] #{"Malaipatukatām" "Malaipatukaṭām" "Malaipaṭukaṭām"}
   "Māmūlaṉār" #{"Māmūlaṇār"}
   "maṉai" #{"manai"}
   "Mangulam" #{}
   "மணி" #{"mani"}
   "Manickam" #{}
   "மணிகள்" #{"manikal"}
   [:t "மாணிக்கவாசகர்'s"] #{}
   [:t "மாங்குடி"] #{"Mānkuţi" "Mānkuți"}
   "மணிமாலை" #{}
   [:t "மணிமேகலை"] #{"Maņimēkalai"}
   "மணிப்ரவாள" #{"maṇipravāļa"}
   "மணிப்ரவாளம்" #{"maṇipravāļaம்"}
   [:t "மங்கையர்க்கரசியின்"] #{"Mankaiyarkkavaciyin" "Mankaiyarkkaraciyin"}
   "maṇpukku" #{"manpukku"}
   "mantic" #{"mantic"}
   "மந்திரம்" #{}
   "Maraimalai" #{}
   "மரணம்" #{}
   "மரபு" #{}
   "Marr" #{}
   "முருக" #{}
   [:t "முருக"] #{}
   [:t "முருகு"] #{}
   "மறுமலர்ச்சி" #{"marumalarcci"}
   "மருந்து" #{}
   "மருதம்" #{}
   [:t "மருதம்"] #{}
   [:t "மருதனார்"] #{"Marutaṇār"}
   [:t "மாதவி"] #{}
   "மடமை" #{}
   "மாத்திரை" #{}
   [:t "மாணிக்கவாசகர்"] #{"Māņikkavācakar"}
   "mātrā" #{}
   "மதுரை" #{}
   [:t "மதுரை"] #{}
   [:t "மதுரைக்"] #{}
   [:t "மதுரைக்காஞ்சி"] #{"Maturaikkañci" "Maturaikkaňci" "Maturaikkānci" "Maturaikkāñci" "Maturaikkāňci"}
   "Mauni" #{}
   "māyvatu" #{}
   "Mayilâpûr" #{}
   "Mayilāpur" #{}  ;; ""English" (Latin script) spelling here doesn't quite match மயிலாப்பூர், but that's how it's spelled in the book
   "Meenakshisundaran" #{}
   "millennium" #{"millenium"} ;; English misspelling
   "millenniums" #{"milleniums"} ;; English misspelling
   "Minnal" #{}
   "monistic" #{}
   "Montaignesque" #{}
   "moralizations" #{}
   "morphophonemic" #{}
   "Mudaliar" #{}
   "Mudaliyar" #{}
   "மூலம்" #{}
   "முல்லை" #{}
   [:t "முல்லைப்"] #{}
   [:t "முல்லைப்பாட்டு"] #{"Mullaippaṭṭu" "Mullaippattu"}
   [:t "மும்மணிக்கோவை"] #{}
   "மும்மணிக்கோவை" #{"mummanikkovai" "mummaņikkovai" "mummaņikkōvai" "mummaṇikkovai" "mummaṇikkövai"}
   "munsif" #{}
   "Murugan" #{}
   "Murukaṉ" #{"Murukan"}
   "Murukaṉ's" #{"Murukan's"}
   "முதல்" #{}
   [:t "முதலியார்"] #{}
   "Mutaliār" #{}
   "முதற்பொருள்" #{"mutarporul"}
   [:t "முதற்பொருள்"] #{"mutarporul"}
   "Muttusami" #{}
   "Mutukoṟ-" #{"Mutukor-"}
   "Mutukoṟ" #{"Mutukor"}
   "Muciṟi" #{"Muciri"}
   "Mylapore" #{}
   [:t "நச்செள்ளையார்"] #{"Naccellaiyar" "Nacceļļaiyār"}
   [:t "நச்சினார்க்கினியர்"] #{"Naccinārkkiniyar" "Nacciṇārkkiniyar" "Nacciṇārkkiṇiyar"}
   [:t "நச்சினார்க்கினியர்'s"] #{"Naccinārkkiniyar's" "Nacciṇārkkiniyar's" "Nacciṇārkkiṇiyar's"}
   "Nagarcoil" #{}
   "Nagaswamy" #{}
   [:t "நக்கீரர்"] #{"Nakkirar"}
   [:t "நக்கீரர்'s"] #{"Nakkirar's"}
   [:t "நக்கீரர்s"] #{"Nakkirars"}
   [:t "நக்கீரதேவர்"] #{}
   [:t "நக்கீரனார்"] #{"Nakkīraṇār"}
   [:t "நல்லியக்கோடன்"] #{"Nalliyakkōṭan"}
   "நமசிவாயம்" #{"namacivayam"}
   "Nampi" #{}
   "Nandakumar" #{}
   "Nandivarman" #{}
   [:t "நன்னன்"] #{"Nannan"}
   [:t "நன்னூல்"] #{"Nannul" "Nannūl"}
   "Nantikkalampakam" #{}
   "nāgarika" #{}
   [:t "நாலடியார்"] #{}
   "nāṇ" #{}
   "ஞானம்" #{"ňāṇam"}
   "நாநூறும்" #{"nānūrum"}
   "நப்பூதனார்" #{"Napputaṇār"}
   [:t "நாராயண்"] #{}
   "Narayanan" #{}
   "நற்றிணை" #{"narrinai" "narriņai" "narvinai" "narviņai"}
   [:t "நற்றிணை"] #{"Narrinai" "Narriņai" "Narvinai"}
   "நட்பு" #{"natpu"}
   "Nattar" #{}
   "Nāṭyaśāstra" #{}
   [:t "நாவலர்"] #{}
   "Nayagam" #{}
   "நாயன்மார்" #{"nāyaṇmār"}
   [:t "நாயன்மார்"] #{"nāyaṇmār"}
   "neo" #{}
   "நேர்" #{}
   [:t "நெடுனல்"] #{"Nețunal"}
   [:t "நெடுனல்வாடை"] #{"Nețunalvāṭai"}
   [:t "நெடுஞ்செழியன்"] #{"Nețuñceliyan" "Nețuňceliyan"}
   "நெய்தல்" #{}
   [:t "நெய்தல்"] #{}
   "Nilakanta" #{}
   "Nilakantan" #{}
   "நிலம்" #{}
   "Nilgiri" #{}
   "Nilgiris" #{}
   "நிறை" #{"nirai"}
   "niṟīit" #{"nirīit"}
   "நூல்" #{"nul"}
   "நூற்பா" #{"nurpā"}
   "நூற்பாs" #{"nurpās"}
   "ஓசை" #{}
   [:t "ஓசை"] #{}
   "ஓம்" #{}
   "ஓரை" #{}
   "ஒரு" #{}
   [:t "ஒட்டக்கூத்தன்"] #{"Oṭṭakküttan" "Oṭṭakkūttan"}
   [:t "ஒட்டக்கூத்தர்"] #{"Oṭṭakküttar" "Oṭṭakkūttar"}
   "outcaste" #{}
   "Padmanabhan" #{}
   "palaeography" #{}
   "paṭṭuṉ" #{"paṭṭun"}
   "பாலை" #{"palai"}
   [:t "பாலை"] #{"Palai"}
   "Pallava" #{}
   "Pallavan" #{}
   "Pallavas" #{}
   "pāṇ" #{}
   "pāṇar" #{"paṇar"}
   "pāṇi" #{}
   "pandit" #{}
   "pandits" #{}
   "Pandya" #{}
   "Pandyas" #{}
   [:t "Pāṇini"] #{"Pāņini"}
   "Pāṇinīan" #{"Pāṇinian"}
   "Panneerselvam" #{}
   "paṇpuṭaiyār" #{"panpuṭaiyar"}
   [:t "பாண்டிக்கோவை"] #{"Pāṇṭikkovai"}
   [:t "பாண்டியன்"] #{"Pāṇṭiyan"}
   "பாண்டியன்" #{"Pāṇṭiyan"}
   "Paraiya" #{}
   [:t "பரணர்"] #{}
   [:t "பாரதம்"] #{}
   "Pārati" #{}
   [:t "பாரி"] #{}
   "Parimēlaḻakar" #{"Parimēlaļakar" "Parimēlalakar"}
   "Parimēlaḻakar's" #{"Parimēlalakar's"}
   "பரிபாடல்" #{"Paripatal" "Paripaṭal" "Paripāțal" "Paripāṭal" "Paripățal"}
   "paṇ" #{}
   "Patanjali" #{}
   "Patanjali's" #{}
   "பதிகம்" #{}
   [:t "பதிகம்"] #{}
   "பதிகம்s" #{}
   [:t "பதிற்"] #{"Patir"}
   [:t "பதிற்றுப்"] #{"Patirrup"}
   [:t "பதிற்றுப்பத்து"] #{"Patirruppattu"}
   [:t "பட்டினப்பலை"] #{"Pattinappalai" "Pattinappālai"}
   [:t "பட்டினத்தர்"] #{"Paṭṭinattar" "Paṭṭinattār" "Paṭṭiṇattār"}
   [:t "பத்தினி"] #{"Pattini" "Pattiņi"}
   "பாட்டு" #{}
   [:t "பத்துப்பாட்டு"] #{"Pattupattu" "Pattuppattu" "Pattuppaṭṭu"}
   "பத்துப்பாட்டு" #{}
   [:t "பவணந்தி"] #{}
   "பாயிரம்" #{}
   [:t "பேராசிரியர்"] #{}
   [:t "பேராசிரியர்'s"] #{}
   [:t "பெரிய"] #{}
   [:t "பெரியபுரணம்"] #{"Periyapurānam" "Periyapuraṇam" "Periyapuranam"}
   [:t "பெரும்பாணாற்றுப்படை"] #{"Perumpāṇārruppatai" "Perumpāṇārruppaṭai"}
   "பெரும்பொழுது" #{"perumpolutu"}
   [:t "பெருந்கதை"] #{"Perunkatai"}
   [:t "பெருங்குன்றூர்"] #{"Perunkunrur"}
   "perumpeyar" #{}
   [:t "பெருஞ்சேரல்"] #{"Peruñcēral"}
   [:t "பெருந்தேவணார்"] #{}
   [:t "பெருந்தேவணார்'s"] #{}
   "பெருந்திணை" #{"peruntiņai"}
   [:t "பெருந்தொகை"] #{}
   "பேய்" #{}
   "phonaesthetic" #{}
   "physiographic" #{}
   "Pichamurti" #{}
   "Piccamūrtti" #{"Piccamurthi" "Piccamürtti"}
   "Pillai" #{}
   "Pillai's" #{}
   [:t "பிள்ளை"] #{"Piļļai"}
   [:t "பிள்ளைத்தமிழ்"] #{"Pillaittamil"}
   "பிள்ளைத்தமிழ்" #{"pillaittamil"}
   "பிள்ளையார்" #{"pillaiyār"}
   [:t "பிள்ளையார்"] #{"Pillaiyār" "Piļļaiyār"}
   "பிராணம்" #{}
   "பிரிதல்" #{}
   "பிரிவு" #{}
   "politikon" #{}
   "pointe" #{}
   "Pondicherry" #{}
   "Pondichery" #{}
   "Pondichéry" #{}
   [:t "பொருள்"] #{"Porul" "Poruļ"}
   "பொருள்" #{"porul" "poruļ"}
   [:t "பொருளதிகாரம்"] #{"Porulatikāram" "Poruļatikāram"}
   [:t "பொருநர்"] #{}
   [:t "பொருநராற்றுப்படை"] #{"Porunararruppatai" "Porunararruppaṭai" "Porunarārruppațai" "Porunarārruppaṭai" "Porunarāṛruppatai" "Porunarāṛruppaṭai" "Porunavärruppaṭai"}
   "பொதுவியல்" #{}
   "pre" #{}
   "prosodic" #{}
   "Proto" #{}
   "pukaḻ" #{"pukal"}
   "pukaḻeṉiṉ" #{"pukalenin"}
   "Pukaḻēnti" #{"Pukaļēnti"}
   "pukaḻnta" #{"pukalnta"}
   "pulaitti" #{}
   "pulavar" #{}
   [:t "புலவர்"] #{}
   "புளியமரத்தின்" #{"puliyamarattin"}
   "puṟam" #{"puram"}
   "Puṟam" #{"Puram"}
   "Puṟ" #{"Pur"}
   "Purāṇam" #{}
   "purāṇam" #{}
   "purāṇas" #{"purāņas" "purānas"}
   "purāṇic" #{}
   "Purāṇic" #{}
   [:t "பறநானூறு"] #{"Puranaṇuru" "Puranaṇūru" "Puranānūru" "Puranānūṛu" "Puranāṇūru" "Puṛanānūru" "Puṛanānūṛu" "Puṛanāṇūru"}
   "புறப்பொருள்" #{"purapporuļ"}
   "Puṟapporuḷ" #{"Purapporul"}
   "Purnalingam" #{}
   "Putumaippittaṉ" #{"Putumaippittan"}
   "Puthumaippittan" #{}
   "Puthumaippitthan" #{}
   "Putra" #{}
   "PVM" #{}
   "Raghava" #{}
   "Raghunathan" #{}
   "Raj" #{}
   "Raja" #{}
   "Rajagopala" #{}
   "Rajagopalachari" #{}
   "Rajagopalan" #{}
   "Rajam" #{}
   "Rām" #{}
   "Rāma" #{}
   "rāma" #{}
   "Rāma's" #{}
   "Ramachandra" #{}
   "Ramalinga" #{}
   [:t "ராமலிங்க"] #{"Rāmalinga" "Rāmalinka"}
   [:t "ராமலிங்கர்"] #{"Rāmalinkar"}
   "Ramamirtham" #{}
   "Ramamirtham's" #{}
   "Ramnad" #{}
   "Rāmānuja" #{}
   "Ramaswami" #{}
   "Rāmatēvar" #{}
   "Rāmāyaṇa" #{"Rāmāyana"}
   "rāmāyaṇam" #{}
   "Rāmāyaṇas" #{}
   "Ranganathamuni" #{}
   "Ranganathan" #{}
   "Ranganāthamuni" #{}
   "rasa" #{}
   "Rāvaṇa" #{}
   "reflexion" #{}
   "religio" #{}
   "Rōma" #{}
   "Sahitya" #{}
   "Saiva" #{}
   "Śaiva" #{"Šaiva"}
   "ŚAIVA" #{}
   "SAIVA" #{}
   "Saivism" #{}
   "Saivite" #{}
   "Saivites" #{}
   "Samājam" #{}
   "sandhi" #{}
   "Sangam" #{}
   "Sanskritic" #{}
   "Sanskritization" #{}
   "Sanskritized" #{}
   "Santhanam" #{}
   "Sarasvati" #{}
   "Saraswathi" #{}
   "Sastri" #{}
   "Śātavāhana" #{"Satavahana"}
   "Śātavāhans" #{"Satavahanas"}
   "Schiffman" #{}
   "scholiast" #{}
   "scholiasts" #{}
   "Schomerus" #{}
   "Seenisami" #{}
   "Selvam" #{}
   "Seshagiri" #{}
   "Shankar" #{}
   "Shanmugam" #{}
   "Shanmugasundaram" #{}
   "Shilappadikaram" #{}
   "Siddha" #{}
   "Siddhānta" #{"Siddhanta"}
   "Siddhar" #{}
   "siddhar" #{}
   "Siddhars" #{}
   "Siddhas" #{}
   "Siddhism" #{}
   "Silappadigáram" #{}
   "Silappadikaram" #{}
   "silentio" #{}
   "Sītā" #{}
   "S.I.S.S." #{}
   "Śiva" #{"Šiva"}
   "Śiva's" #{}
   "Sivaraja" #{}
   "Sivaramu" #{}
   "Skanda" #{}
   "socio" #{}
   "Somasundara" #{}
   "Somasundaram" #{}
   "Sreenivasan" #{}
   "Śrî" #{}
   "Śrī" #{}
   "Srinivas" #{}
   "Srinivasa" #{}
   "stanzaic" #{}
   "Subbiah" #{}
   "Subrahmanian" #{}
   "Subrahmaniya" #{}
   "Subrahmanya" #{}
   "Subrahmanyam" #{}
   "Subramania" #{}
   "Subramaniam" #{}
   "Subramanya" #{}
   "Subramanyam" #{}
   "Subramoniam" #{}
   "sujet" #{}
   "sujets" #{}
   "Sundara" #{}
   "Sundararajan" #{}
   "superstratum" #{}
   "sūtra" #{"sutra"}
   "sūtras" #{"sutras"}
   "Svami" #{}
   "Svāmī" #{}
   "Swaminatha" #{}
   "synchronic" #{}
   "synchronism" #{}
   "Synchronism" #{}
   [:t "தலைச்சங்கம்"] #{"Talaiccankam"}
   "தலைச்சங்கம்" #{"talaiccankam"}
   "தலைவன்" #{"talaivan"}
   "தமிழ்" #{"tamil"}
   [:t "தமிழகம்"] #{"Tamilakam"}
   [:t "தமிழ்ச்"] #{"Tamilc"}
   "Tamilian" #{}
   "Tamilnad" #{}
   [:t "தமிழ்நாடு"] #{"Tamilnāțu"}
   [:t "தாமோதரம்"] #{}
   "tamoul" #{}
   "Tamoul" #{}
   "tamoule" #{}
   "tamouls" #{}
   "tampukaḷ" #{"tampukaļ"}
   "தனித்தமிழ்" #{"tanittamil"}
   "Tanjavur" #{}
   "Tanjore" #{}
   "தண்டியலங்காரம்" #{"Tantiyalankaram" "Tantiyalaṁkāram"}
   "tantric" #{}
   "taṭāri" #{}
   [:t "தாயுமானவர்"] #{"Tāyumāṇavar"}
   "Terminalia" #{}
   [:t "தேவகுலத்தார்"] #{}
   [:t "தேவாரம்"] #{}
   "Thani" #{}
   "Thiru" #{}
   "Thooran" #{}
   "திணை" #{"tinai" "tiņai"}
   "திணைs" #{"tinais" "tiņais"}
   [:t "திணை"] #{"Tinai" "Tiņai"}
   [:t "திணைs"] #{"Tinais" "Tiņais"}
   [:t "திரணதூமாக்கினி"] #{"Tiraṇatūmākkini" "Tiraṇatūmākkiņi"}
   "Tirukkuṟaḷ" #{"Tirukkural" "Tirukkuraļ"}
   "Tiru" #{}
   "Tiruk" #{}
   [:t "திருக்கோவயார்"] #{"Tirukkōvaiyar"}
   [:t "திருமாலிசை"] #{"Tirumalicai"}
   [:t "திருமங்கை"] #{"Tirumankai"}
   "Tirumantiram" #{}
   [:t "திருமுறை"] #{"Tirumurai"}
   [:t "திருமுருகாற்றுப்படை"] #{"Tirumurukärruppatai" "Tirumurukärruppaṭai" "Tirumurukārṛuppațai" "Tirumurukārṛuppaṭai" "Tirumurukāṛruppatai" "Tirumurukāṛruppaṭai"}
   "Tirumūlar" #{}
   "Tirumūlar's" #{}
   "திருநாவுக்கரசர்" #{"Tirunavukkaracar"}
   "Tirunelveli" #{}
   "Tirupati" #{}
   [:t "திருப்புகழ்"] #{"Tiruppukal"}
   [:t "திருத்தணி"] #{"Tiruttani"}
   [:t "திருத்தொண்டத்தொகை"] #{"Tiruttontattokai"}
   [:t "திருவள்ளுவமாலை"] #{"Tiruvalluvamalai" "Tiruvalluvamālai"}
   ;;  [:t "திருவள்ளுவர்"] #{"Tiruvalluvar"} # sometimes written with diacritics, sometimes not (esp when quoting someone else)
   [:t "திருவள்ளுவர்'s"] #{"Tiruvalluvar's"}
   "திருவந்தாதி" #{"tiruvantāti"}
   [:t "திருவந்தாதி"] #{"Tiruvantāti"}
   "Tiruvaṇṇāmalai" #{}
   "Tiruvācakam" #{}
   "Tiruvārūr" #{}
   "Tiruvātavūrār" #{}
   [:t "திருவேறகம்"] #{"Tiruvēṛakam" "Tiruvērakam"}
   "Tiruvicaippā" #{"Tiruvicaippa"}
   [:t "திருவொற்றியூர்"] #{"Tiruvorriyūr" "Tiruvorriyür" "Tiruvorriyur"}
   "Toda" #{}
   "Todas" #{}
   "தொகை" #{}
   "தொல்" #{}
   "Tolk" #{}
   "Tolkāppiyam" #{"Tolkappiyam" "Tolkäppiyam"}
   "tolkāppiyam" #{"tolkäppiyam"}
   [:t "தொல்காப்பியன்"] #{"Tolkäppiyan" "Tolkāppiyan"}
   [:t "தொல்காப்பியன்'s"] #{"Tolkäppiyan's" "Tolkāppiyan's"}
   "TOLKĀPPIYAM" #{}
   "Tolkāppiyar" #{}
   [:t "தொல்காப்பியனார்"] #{"Tolkāppiyaṇār"}
   "தொண்டன்" #{"tontan"}
   "தொண்டர்" #{"tontar" "toṇṭar"}
   [:t "தொண்டைமான்"] #{"Toṇṭaimāṇ"}
   [:t "தொண்டி"] #{"Tonti" "Tonți"}
   "தொடை" #{"toțai" "toṭai"}
   [:t "தொடை"] #{"Toțai"}
   "toto" #{}
   "Trivandrum" #{}
   "trobadors" #{}
   "tāmmāyntaṉar" #{"tāmmāyntaṇar"}
   "tulakam" #{}
   "Tulu" #{}
   "துறை" #{"turai"}
   [:t "துறை"] #{"Turai"}
   [:t "துரைசாமி"] #{"Turaicāmi" "Turaicami"}
   "துறவறம்" #{"turavaram"}
   "உலகம்" #{}
   "உழவர்" #{"ulavar"}
   "உள்ளுரை" #{"ullurai"}
   "und" #{}
   "unimpassioned" #{}
   "urai" #{}
   "உரையாசிரியர்" #{"uraiyäciriyar"}
   "உரையாசிரியர்கள்" #{"Uraiyāciriyarkal" "Uraiyāciriyarkaļ"}
   "Uṟaiyūr" #{"Uraiyur" "Uraiyūr"}
   "உரி" #{}
   [:t "உரி"] #{}
   "உரிப்பொருள்" #{"uripporul"}
   "Urtext" #{}
   [:t "உருத்திரன்"] #{"Uruttiran"}
   "உரு" #{}
   "உருவு" #{}
   [:t "உதியன்"] #{"Utiyan" "Utiyaň"}
   "உவமம்" #{}
   "உயிர்" #{}
   "uyiruṅ" #{"uyiruń"}
   [:t "வசந்தமாலை"] #{}
   [:t "வைகை"] #{}
   "Vaiṣṇava" #{}
   "Vaiṣṇavism" #{}
   "Vaiṣṇavite" #{}
   "Vaitheeswaran" #{}
   "Vaiyapuri" #{}
   [:t "வள்ளி"] #{"Valli"}
   "Vallikkannan" #{}
   "வள்ளுவன்" #{"valluvan"}
   ;; "Valluvar" sometimes with diacritics, sometimes not
   "Vālmīki" #{"Vālmiki"}
   "வஞ்சி" #{"vanci" "vañci"}
   [:t "வஞ்சி"] #{"Vanci" "Vañci"}
   "வண்ணம்" #{"vannam"}
   "வந்தால்" #{"vantāļ"}
   "வந்தொண்டர்" #{"vantontar"}
   "Varadarajan" #{}
   "வரலாறு" #{"varalāru" "varalāṛu"}
   "வட்டம்" #{}
   "Vedanayagam" #{}
   "Vedic" #{}
   "Venkatacalam" #{}
   "Venkatachalam" #{}
   "Venkataswamy" #{}
   "வெண்பா" #{"venpā"}
   [:t "வெண்பா"] #{"Venpā"}
   "veṇpāmālai" #{"veņpāmālai"}
   "Venugopala" #{}
   "Venugopalan" #{}
   "venus" #{}
   "Vijaya" #{}
   "Vijayanagara" #{}
   "viḻuppuṇ" #{"viluppun"}
   "viṟaliyar" #{"viraliyar"}
   "விருத்தம்" #{}
   "Vithiananthan" #{}
   "விளக்கு" #{"viļakku"}
   "Viṣṇu" #{}
   "v.l." #{}
   "Volksliteratur" #{}
   "von" #{}
   "vākai" #{}
   "Wellek" #{}
   "Winternitz" #{}
   "worldly" #{"wordly"}
   "yāḻ" #{"yal"}
   "yātum" #{}
   ;; "Yavanas" sometimes with and sometimes without diacritics
   "yāvaruṅ" #{"yāvarun"}
   "Zbavitel" #{}
   "Zvelebil" #{}
   "zoon" #{}
   "élite" #{}
   "āṭunar" #{}
   "ūrē" #{}
   ;; abbreviations & names 
   "10" "IO"
   "101" "IOI"
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
   "Damodaram" #{}
   "Daniélou" #{}
   "dans" #{}
   "dateable" #{}
   "de" #{}
   "DBIA" #{}
   "DED" #{}
   "Ded" #{}
   "der" #{}
   "des" #{}
   "Desikan" #{}
   "Dessigane" #{}
   "Deva" #{}
   "Digambara" #{}
   "Dikshitar" #{}
   "Dikshitar's" #{}
   "distichs" #{}
   "Doraiswami" #{}
   "Doraiswamy" #{}
   "Dravidians" #{}
   "Dupleix" #{}
   "Duraiswami" #{}
   "e.g" #{}
   "E.g." #{}
   "Eliade" #{}
   "Emeneau" #{}
   "Emeneau's" #{}
   "Engl" #{}
   "François" #{}
   "ftn" #{}
   "Glasenapp" #{}
   "Glazov" #{}
   "Gnanasambandan" #{}
   "Gondi" #{}
   "Gonzalves" #{}
   "Gooroo" #{}
   "Gopalie" #{}
   "Gover" #{}
   "Graeco" #{}
   "Govinda" #{}
   "Govindaraja" #{}
   "Gros" #{}
   "Guha" #{}
   "Gōpālakṛṣṇa" #{}
   "Hamm" #{}
   "Hanumān" #{"Hanuman"}
   "Hari" #{}
   "henolocotheism" #{}
   "henolocotheistic" #{}
   "Henrique" #{}
   "Henriques" #{}
   "Higginbotham" #{}
   "Hoole" #{}
   "HSI" #{}
   "HTL" #{}
   "HTLL" #{}
   "ib" #{}
   "II2" #{}
   "II3" #{}
   "II4" #{}
   "IIJ" #{}
   "Introd" #{}
   "Iravatham" #{}
   "Iyengar" #{}
   "Iyer" #{}
   "Iyer's" #{}
   "Jagannathan" #{}
   "Jaffna" #{}
   "Jains" #{}
   "Janakiraman" #{}
   "Jesudasan" #{}
   "Jesudasans" #{}
   "Jeyakanthan" #{}
   "Jeyakanthan's" #{}
   "Kur" #{}
   "K.Z." #{}
   "Ka" #{}
   "Kalit" #{}
   "Kamil" #{}
   "Kannadasan" #{}
   "Kannappa" #{}
   "Nar" #{}
   "Ramaswamy" #{}
   "e.g." #{}
   "i.e." #{}
   "lw" #{}
   "Skt" #{}
   "ss" #{}
   "Ss" #{}
   "TP" #{}
   ;; puncutation
   "”" #{}
   "“" #{}
   "—" #{}
   ;; REMOVE
   ;;   (relies on the fact that number of repeated whitespace is not significant in Markdown)
   ;;   Note: we have the 'remove' category because of a human marking in the margin that got interpreted as a "×"
   "" #{"×"}})

(defn f
  [k]
  (let [[f token] (if (string? k)
                    [identity k]
                    (let [[fk token] k]
                      [(case fk
                         :u str/upper-case
                         :t str/capitalize)
                       token]))
        k-iso15919 (translit/தமிழ்->iso15919 token)]
    (f k-iso15919)))

(def CORRECT-SPELLINGS-LATINIZED
  (update-keys CORRECT-SPELLINGS f))

(def SPELLING-CORRECTIONS
  (into {} (for [[k vs] CORRECT-SPELLINGS-LATINIZED
                 v vs]
             [v k])))

"cāṉṟōr" ;; Elango edition - NFD form
"cāṉṟōr" ;; Internet inspired - NFC form

(defn keep-misspelled-non-english-words
  [word-index-scores]
  (let [trimmed-words (map #(update-in % [:substring] string/trim) word-index-scores)
        no-punctuation (remove #(re-matches #"[\p{Space}\p{Punct}]+" (:substring %)) trimmed-words)
        no-english (remove #(d/is-word? (:substring %)) no-punctuation)
        no-correct-non-english (remove #(CORRECT-SPELLINGS-LATINIZED (:substring %)) no-english)
        no-visited-incorrect-non-english (remove #(SPELLING-CORRECTIONS (:substring %)) no-correct-non-english)]
    ;; TODO: remove all words of a high confidence score that match the English dictionary
    no-visited-incorrect-non-english))

(defn format-word-index-score-output
  [word-index-score]
  (str (pr-str (:substring word-index-score))
       \tab
       (pr-str (:transliteration word-index-score))
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
