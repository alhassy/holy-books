<h1> An Emacs interface to the Quran and the Bible: Org-mode links, tooltips, and Lisp look-ups </h1>

<div align="center">

</div>

<div align="center">

Let's use Org-mode links to look-up Quranic and Biblical verses!

“Live” examples & documentation: <https://alhassy.github.io/holy-books/>

<a href="https://github.com/alhassy/holy-books"><img src="https://img.shields.io/badge/holy--books-1.0-informational?logo=Gnu-Emacs"></a>

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


<a id="Installation-Instructions"></a>

## Installation Instructions

Manually or using [quelpa](https://github.com/alhassy/emacs.d#installing-emacs-packages-directly-from-source):

    ;; ⟨0⟩ Download the holy-books.el file manually or using quelpa
    (quelpa '(holy-books :fetcher github :repo
    "alhassy/holy-books"))

    ;; ⟨1⟩ Have this always active in Org buffers
    (add-hook #'org-mode-hook #'holy-books-mode)

    ;; ⟨1′⟩ Or use: “M-x holy-books-mode” to turn it on/off


<a id="Bye"></a>

## Bye!

<img src="https://img.shields.io/badge/thanks-for_reading-nil?logo=nil">
<a href="https://twitter.com/intent/tweet?text=This looks super neat (•̀ᴗ•́)و::&url=https://github.com/alhassy/holy-books"><img src="https://img.shields.io/twitter/url?url=https://github.com/alhassy/holy-books"></a>
<a href="https://www.buymeacoffee.com/alhassy"><img src="https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee"></a>
