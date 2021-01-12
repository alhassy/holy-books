<h1> An Emacs interface to the Quran and the Bible: Org-mode links, tooltips, and Lisp look-ups </h1>

<div align="center">

</div>

<div align="center">

Let's use Org-mode links to look-up Quranic and Biblical verses!

“Live” examples & documentation: <https://alhassy.github.io/holy-books/>

<a href="https://github.com/alhassy/holy-books"><img src="https://img.shields.io/badge/holy--books-1.3-informational?logo=Gnu-Emacs"></a>

<a href="https://twitter.com/intent/tweet?text=This looks super neat (•̀ᴗ•́)و::&url=https://github.com/alhassy/holy-books"><img src="https://img.shields.io/twitter/url?url=https://github.com/alhassy/holy-books"></a>
<a href="https://github.com/alhassy/holy-books/issues"><img src="https://img.shields.io/badge/contributions-welcome-green?logo=nil"></a>

<a href="https://alhassy.github.io/"><img src="https://img.shields.io/badge/author-musa_al--hassy-purple?logo=nintendo-3ds"></a>
<a href="https://www.buymeacoffee.com/alhassy"><img src="https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee"></a>

<a href="https://alhassy.github.io/about"><img src="https://img.shields.io/badge/Hire-me-success?logo=nil"></a>

</div>


# Table of Contents

1.  [Short Example](#Short-Example)
2.  [Long Example](#Long-Example)
3.  [Summary](#Summary)
    1.  [Installation Instructions](#Installation-Instructions)
    2.  [Bye!](#Bye)


<a id="Short-Example"></a>

# Short Example

![img](images/short_example.png)


<a id="Long-Example"></a>

# Long Example

![img](images/long_example.png)


<a id="Summary"></a>

# Summary

> The full article may be read as
> <a href="https://alhassy.github.io/holy-books/"><img src="https://img.shields.io/badge/-HTML-informational?logo=ghost"></a> &#x2014;or visit the
> repo <a href="https://www.github.com/alhassy/holy-books/stars"><img src="https://img.shields.io/github/stars/alhassy/holy-books?style=social"></a> .



<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Link</th>
<th scope="col" class="org-left">Action</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>quran:chapter:verse</code></td>
<td class="org-left">Retrive a verse from the Quran</td>
</tr>


<tr>
<td class="org-left"><code>bible:book:chapter:verse</code></td>
<td class="org-left">Retrive a verse from the Bible</td>
</tr>


<tr>
<td class="org-left"><code>[[basmala:]]</code></td>
<td class="org-left">Produce the Basmala ligature</td>
</tr>
</tbody>
</table>

These each take optional arguments separated by ‘|’; see <holy-books-quran>
and <holy-books-bible> or see the full documentation online at
<a href="https://alhassy.github.io/holy-books/"><img src="https://img.shields.io/badge/-HTML-informational?logo=ghost"></a>.

There are also <holy-books-insert-quran> and <holy-books-insert-bible> to
inject verses in the current Emacs buffer ;-)

Moreover, the Quran's translation and the Bible's version can both be selected&#x2026;

<details class="code-details"
                 style ="padding: 1em;
                          background-color: #e5f5e5;
                          /* background-color: pink; */
                          border-radius: 15px;
                          color: hsl(157 75%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;">
                  <summary>
                    <strong>
                      <font face="Courier" size="3" color="green">
                         ‘holy-books-quran’ details
                      </font>
                    </strong>
                  </summary>

    (documentation #'holy-books-quran)

    Lookup a verse, as a string, from the Quran.

    CHAPTER and VERSE are both numbers, referring to a chapter in the Quran
    and a verse it contains.
    In the associated Org link, both are treated as strings.

    + Lookups are stored in the variable ‘holy-books-quran-cache’ for faster reuse.
    + Quran lookup is based on https://quran.com .
    + Examples:

        ;; Get verse 2 of chapter 7 of the Quran
        (holy-books-quran 7 2)

        ;; Get English-Arabic name of 7th chapter
        (cl-getf (cl-getf holy-books-quran 7) :name)

    The particular translation can be selected by altering the
    HOLY-BOOKS-QURAN-TRANSLAITON variable.

    --------------------------------------------------------------------------------

    There is an Org link form: “quran:chapter:verse|color|size|no-info-p”
    Only ‘chapter’ and ‘verse’ are mandatory; when ‘no-info-p’ is given,
    the chapter and verse numbers are not mentioned in the resulting output.

    Examples:
               quran:7:157|darkgreen|30px|t

               quran:7:157

    For now, only Org HTML export is supported.

    --------------------------------------------------------------------------------

    Finally, there is also an HTML tooltip version with a captial ‘Q’;
    it takes the same arguments but only the chapter and verse are actually used.
    E.g. Quran:7:157 results in text “Quran 7:157” with a tooltip showing the verse.


</details>

<details class="code-details"
                 style ="padding: 1em;
                          background-color: #e5f5e5;
                          /* background-color: pink; */
                          border-radius: 15px;
                          color: hsl(157 75%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;">
                  <summary>
                    <strong>
                      <font face="Courier" size="3" color="green">
                         ‘holy-books-bible’ details
                      </font>
                    </strong>
                  </summary>

    (documentation #'holy-books-bible)

    Retrive a verse from the Christian Bible.

    CHAPTER is a number.
    VERSES is either a number or a string “x-y” of numbers.
    BOOK is any of the books of the Bible, with ‘+’ instead of spaces!

    Examples:

            (holy-books-bible "Deuteronomy" 18 "18-22")  ;; Lisp

            bible:Deuteronomy:18:18-22|darkblue   ;; Org-mode

            Bible:Deuteronomy:18:18-22            ;; Tooltip

    There is also an Org HTML export link, “bible:book:chapter:verse”
    sharing the same optional arguments and variations as the “quran:” link;
    see the documentation of the method HOLY-BOOKS-QURAN for details.

    The particular version can be selected by altering the
    HOLY-BOOKS-BIBLE-VERSION variable.

    Currently, Bible lookups are not cached and Quran lookups do not support the
    “x-y” verse lookup style.

    Possible books include:

     ;; Old Testament
     Genesis Exodus Leviticus Numbers Joshua Judges Ruth
     1+Samuel 2+Samuel 1+Kings 2+Kings 1+Chronicles 2+Chronicles Ezra
     Nehemiah Esther Job Psalms Proverbs Ecclesiastes Song+of+Solomon
     Isaiah Jeremiah Lamentations Ezekiel Daniel Hosea Joel Amos
     Obadiah Jonah Micah Nahum Habakkuk Zephaniah Haggai Zechariah
     Malachi
     ;; New Testament
     Matthew Mark Luke John Acts Romans 1+Corinthians 2+Corinthians
     Galatians Ephesians Philippians Colossians 1+Thessalonians
     2+Thessalonians 1+Timothy 2+Timothy Titus Philemon Hebrews James
     1+Peter 2+Peter 1+John 2+John 3+John Jude Revelation

    For example, the following incantation yields the first verse of
    the first chapter of each book.

       (s-join "

    <hr>" (--map (holy-books-bible it 1 1) ’(...above list...)))


</details>


<a id="Installation-Instructions"></a>

## Installation Instructions

Manually or using [quelpa](https://github.com/alhassy/emacs.d#installing-emacs-packages-directly-from-source):

    ;; ⟨0⟩ Download the holy-books.el file manually or using quelpa
    (quelpa '(holy-books :fetcher github :repo
    "alhassy/holy-books"))

    ;; ⟨1⟩ Have this always active in Org buffers
    (add-hook #'org-mode-hook #'holy-books-mode)

    ;; ⟨1′⟩ Or use: “M-x holy-books-mode” to turn it on/off

    ;; ⟨2⟩ Configure the Quranic translation and Bible version
    ;;     Press ‘C-h o’ to get more info on each variable.
    (setq holy-books-quran-translation "131"  ;; The Clear Quran
          holy-books-bible-version     "niv") ;; New International Version


<a id="Bye"></a>

## Bye!

<img src="https://img.shields.io/badge/thanks-for_reading-nil?logo=nil">
<a href="https://twitter.com/intent/tweet?text=This looks super neat (•̀ᴗ•́)و::&url=https://github.com/alhassy/holy-books"><img src="https://img.shields.io/twitter/url?url=https://github.com/alhassy/holy-books"></a>
<a href="https://www.buymeacoffee.com/alhassy"><img src="https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee"></a>
