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

## Design Considerations

### Producing Output

Options:

1. Concatenating strings of Markdown directly
2. Producing Hiccup (Clojure HTML data) and rendering that to HTML directly
3. Creating map-based AST tree using the Clojure representation of the [Pandoc intermediate JSON form](https://pandoc.org/filters.html), 
as described by the [`pandoc` section of `nextjournal/markdown`](https://nextjournal.github.io/markdown/notebooks/pandoc/)
4. Create the map-based tree using Clojure data that is the input to the `md-type->transform` function from [`nextjournal/markdown`](https://github.com/nextjournal/markdown) that will produce the same output described in the previous option.

We will start with Option 1 (concatening strings of Markdown directly)
since it is simplest given the information we get from the API response JSON.
The API response JSON data gives us the entire plain text
of the OCR output along with segment boundaries of text,
and information about styles (if they apply) of those segments.

### Footnotes

Response object gives information per page.
A page in the response object is a PDF (input doc) page.
However, the Smile of Murugan PDF had scanned two book pages per PDF page.
Therefore, book page != input PDF page.

Within a response object page, it provides 3 different granularities of information of interest:
- blocks
- paragraphs
- tokens

Blocks are continuous stretches of text that are not separated by an empty line or other noticeable distinguishing whitespace, etc.
Paragraphs are computed from the blocks.
In other words, paragraphs are subdivisions from the blocks.
Tokens are more or less all of the words from beginning to end on the page.

We think that footnotes can be identified by looking for blocks that begin with a number (almost always "1") followed by a space. There will be text after the space.
However, page numbers are also detected in their own blocks, but empirically, they are followed only by a newline character.
There is an instance of a footnote that is so large that is split across two pages.
Footnotes seem to be numbered by the publisher resetting to 1 after each page.
There is an instance of a page with multiple footnotes, and in that case, the numbering went up to 2.
In most cases, the footnote is the only one on the page, so the footnote number is "1".

