;; Dependencies ---`s', `dash', and `snap' (with its `fancy-C-x-C-e')--- are installed by the CI workflow below;
;; snap is not yet on MELPA, so we expect clones of it in ~/snap and ~/fancy-C-x-C-e.
(package-initialize)
(add-to-list 'load-path (expand-file-name "~/snap"))
(add-to-list 'load-path (expand-file-name "~/fancy-C-x-C-e"))
(require 'snap)
(snap-define-fixture deftest) ;; The identity fixture: plain ERT tests with natural-language names
(require 's)

(load-file "holy-books.el")

;; Run all with M-x ert RET (tag html-export) RET; refresh a stale :html with C-u C-x C-e on its test.
(snap-define-relation html-export (org html)
  "Exporting the ORG snippet, body-only, yields HTML."
  (let ((actual (org-export-string-as org 'html :body-only)))
    (should (equal html actual))
    (list :html actual)))

(require 'ox-md)
(snap-define-relation md-export (org md)
  "Exporting the ORG snippet, body-only and without a table of contents, yields Markdown MD."
  (let ((actual (org-export-string-as org 'md :body-only '(:with-toc nil))))
    (should (equal md actual))
    (list :md actual)))

(define-html-export-test "basmala with no arguments has an empty colour"
  :org "[[basmala:]]"
  :html "<p>
<center style=\"color:;font-size:60px;padding:25px\">
                     ﷽
                </center></p>
")

(define-html-export-test "basmala with a colour argument uses it"
  :org "basmala:green"
  :html "<p>
<center style=\"color:green;font-size:60px;padding:25px\">
                     ﷽
                </center></p>
")

(deftest "quran looks up a verse in the default translation, The Clear Quran"
  (should (equal (holy-books-quran 1 1) "In the Name of Allah—the Most Compassionate, Most Merciful.")))

(deftest "quran falls back to the public API, and Saheeh International, when the page JSON fails"
  (cl-letf (((symbol-function 'holy-books--quran-from-page) #'ignore))
    (let (holy-books-quran-cache)
      (should (equal (holy-books-quran 1 1)
                     "In the name of Allāh, the Entirely Merciful, the Especially Merciful.")))))

(deftest "quran drops footnote markers"
  (should-not (s-contains? "<sup" (holy-books-quran 7 157))))

(deftest "quran caches the chapter name alongside the verse"
  (holy-books-quran 112 3)
  (should (equal (cl-getf (cl-getf holy-books-quran-cache 112) :name) "Al-Ikhlas The Sincerity")))

(define-html-export-test "quran link with |colour|size|no-info-p styles the verse and omits its reference"
  :org "quran:1:1|darkgreen|30px|t"
  :html "<p>
<span style=\"color:darkgreen;font-size:30px;\">
                             ﴾<em> In the Name of Allah—the Most Compassionate, Most Merciful.</em>﴿ 
                       </span></p>
")

(define-html-export-test "Quran link is a tooltip carrying the verse and chapter name"
  :org "Quran:1:1"
  :html "<p>
<abbr class=\"tooltip\"
                             title=\"﴾<em> In the Name of Allah—the Most Compassionate, Most Merciful.</em>﴿ <br><br> Al-Fatihah The Opener <br><br> https://quran.com/1\">
                          Quran 1:1
                       </abbr>&emsp13;</p>
")

(define-md-export-test "quran link is a blockquote followed by its reference"
  :org "quran:1:1"
  :md "
> In the Name of Allah—the Most Compassionate, Most Merciful.
>
> [Quran 1:1 Al-Fatihah The Opener](https://quran.com/chapter_info/1)

")

(define-md-export-test "Quran link is a Markdown link whose title is the verse and chapter name"
  :org "Quran:1:1"
  :md "[Quran 1:1](https://quran.com/1 \"In the Name of Allah—the Most Compassionate, Most Merciful. - Al-Fatihah The Opener\")

")

(define-html-export-test "quran link shows the verse and its chapter name"
  :org "quran:1:1"
  :html "<p>
<span style=\"color:nil;font-size:nil;\">
                             ﴾<em> In the Name of Allah—the Most Compassionate, Most Merciful.</em>﴿ <small><a href=\"https://quran.com/chapter_info/1?local=en\">Quran 1:1, Al-Fatihah The Opener</a></small>
                       </span></p>
")

(deftest "bible looks up a verse"
  (should (equal (holy-books-bible "Genesis" 1 "1")
                 "In the beginning God created the heavens and the earth.")))

(deftest "bible looks up a verse range, dropping the verse numbers"
  (should (equal (holy-books-bible "Deuteronomy" 18 "18-19")
                 "I will raise up for them a prophet like you from among their fellow Israelites, and I will put my words in his mouth. He will tell them everything I command him. I myself will call to account anyone who does not listen to my words that the prophet speaks in my name.")))

(define-html-export-test "Bible link is a tooltip carrying the verse"
  :org "Bible:Genesis:1:1"
  :html "<p>
<abbr class=\"tooltip\"
                             title=\"﴾<em> In the beginning God created the heavens and the earth.</em>﴿ <br><br> https://www.christianity.com/bible/bible.php?q=Genesis+1\">
                         Genesis 1:1
                       </abbr>&emsp13;</p>
")

(define-md-export-test "bible link is a blockquote followed by its reference"
  :org "bible:Genesis:1:1"
  :md "
> In the beginning God created the heavens and the earth.
>
>[Genesis 1:1](https://www.christianity.com/bible/bible.php?q=Genesis+1&ver=niv)

")

(define-md-export-test "Bible link is a Markdown link whose title is the verse"
  :org "Bible:Genesis:1:1"
  :md "[Genesis 1:1](https://www.christianity.com/bible/bible.php?q=Genesis+1 \"In the beginning God created the heavens and the earth.\")

")

(define-html-export-test "bible link shows the verse and its reference"
  :org "bible:Genesis:1:2"
  :html "<p>
<span style=\"color:nil;font-size:nil;\">﴾<em> Now the earth was formless and empty, darkness was over the surface of the deep, and the Spirit of God was hovering over the waters.</em>﴿<small><a href=\"https://www.christianity.com/bible/bible.php?q=Genesis+1&ver=niv\">Genesis 1:2</a></small> </span></p>
")
