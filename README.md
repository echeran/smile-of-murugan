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