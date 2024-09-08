# The Smile of Murugan

This project uses [timothypratley/happyapi](https://github.com/timothypratley/happyapi)'s
easy-to-use Clojure wrappers around Google APIs in order to
perform OCR (convert images to text)
on the [public domain book _The Smile of Murugan_ by Kamil Zvelebil](https://archive.org/details/smileofmuruganontamilliteratureofsouthindiakamilzvelebilbrill_368_E).

The automatic text conversation provided by the Internet Archive
is not useful since the language was set incorrectly.
As a workaround, the code in this repo will use one of the Google APIs
to perform the OCR again,
but with the correct language being detected.

Other issues that need to be handled in the digitization include "dehyphenation"—removing the hyphens when words wrap across the end of a line,
or across the end of a page.
This dehyphenation must also account for and preserve hyphens in compound words
such as "new-fangled".
Another issue is that Tamil (and Sanskrit) words are written in the Latin script,
just like the rest of the book, which is in English.
These Tamil and Sanskrit words use the [ISO 15919](https://en.wikipedia.org/wiki/ISO_15919)
method of transliteraiton.
The diacritics required for ISO 15919 might not get detected correctly,
so those words will need to be corrected.

## Prerequisites

* Java 11 or greater
* clojure-lsp
  -  [Installation of CLI](https://clojure-lsp.io/installation/)
    * [Usage instructions](https://clojure-lsp.io/api/cli/) 
    * The commands `clojure-lsp format`, `clojure-lsp clean-ns`, and `clojure-lsp diagnostics` should pass
  - [Installation of IDE plugins](https://clojure-lsp.io/clients/)
    *  [Plugin for IntelliJ](https://github.com/clojure-lsp/clojure-lsp-intellij)

## Why Happy API?

The Internet Archive incorrectly tagged this book's language as Sansrkit, not English.
And Sansrkit is not even the subject matter language, which is Tamil.
That means that the plain text download of the text offered by the Internet Archive is in Devanagari script, not English.

Also, for whatever reason, the OCR software used by the Internet Archive to create the PDF of the image scans does overlay the plain text of the scanned text on top of the PDF where the text occurs.
However, whatever OCR software is being used has shortcomings:
* diacritics are not preserved
* there are spaces introduced or deleted (including hyphens at the end of a line when a word wraps a line)
* etc.

Therefore, converting the PDF using PDF parsing tools / editors (ex: Adobe Acrobat) to a rich text format that can handle text and styling (such as DOCX) will not be able to fix those pre-existing problems in the original PDF.

However, Google's OCR software implicitly used by DocAI and other APIs does a much better job:
* many diacritics in the plain text are preserved
* fewer spaces introduced/deleted
* better detection of symbols like hyphens, quote marks, etc.
* more accurate detection of which text subsequences have styling