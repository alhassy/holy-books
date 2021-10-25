(setq needed-libraries
      '(s cl-lib dash org undercover seq quelpa))

(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg needed-libraries)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defmacro deftest (desc &rest body)
  `(ert-deftest ,(intern
;; Convert all non-letters to ‘_’
;; A = 65, z = 122
(concat (seq-map (lambda (c) (if (<= 65 c 122) c ?_))
         desc))) () ,@body))
;; without the s-replace, “M-x ert” crashes when it comes to selecting the test to run.


;; https://github.com/Wilfred/propcheck
(quelpa '(propcheck :fetcher github :repo "Wilfred/propcheck"))
(require 'propcheck)
(when nil ;; example use
  (let ((propcheck-seed (propcheck-seed)))
    (propcheck-generate-string nil)))

;; An attempt to make multiline strings less ugly
(require 's)
(defun unindent (s)
"Allow multiline strings, ignoring any initial indentation (as in Ruby).

The first line of S must be an empty line.

For instance,

(unindent \"
     Hello
       and then some\")

Returns the string:

Hello
  and then some

Notice that the initial indentation has been stripped uniformally
across all lines: The second line begins 2 characters indentated
from the first."
  (let ((indentation (length (car (s-match "\\( \\)+" (cadr (s-split "\n" s)))))))
    (s-chop-prefix "\n"
                   (replace-regexp-in-string (format "^ \\{%s\\}" indentation) "" s))))


(defalias '§ #'unindent)
(§ "
   Hello
       and then some")


(load-file "holy-books.el")

(ert-deftest basmala/empty ()
  (should (equal
           (org-export-string-as "[[basmala:]]" 'html :body-only)
           (unindent
             "<p>
             <center style=\"color:;font-size:60px;padding:25px\">
                                  ﷽
                             </center></p>
             "))))

(ert-deftest basmala/green ()
  (should (equal
           (org-export-string-as "basmala:green" 'html :body-only)
           (unindent
             "<p>
             <center style=\"color:green;font-size:60px;padding:25px\">
                                  ﷽
                             </center></p>
             "))))

(ert-deftest quran/lisp/1:1 ()
  (should (equal
           (holy-books-quran 1 1)
           "
    In the Name of Allah—the Most Compassionate, Most Merciful.
")))

(ert-deftest quran/link/1:1 ()
    :expected-result :failed
  (should (equal
           (org-export-string-as "quran:1:1" 'html :body-only)
           "<p>
<span style=\"color:nil;font-size:nil;\">
                             ﴾<em>
    In the Name of Allah—the Most Compassionate, Most Merciful.
</em>﴿ <small><a href=\"https://quran.com/chapter_info/1?local=en\">Quran 1:1, Al-fatihah The Opener</a></small>
                       </span></p>
")))

(ert-deftest bible/lisp/Genesis:1:2 ()
  (should (equal
           (holy-books-bible  "Genesis" 1 "1")

"
                        In the beginning God created the heavens and the earth.                    "
)))

(ert-deftest bible/link/Genesis:1:2 ()
 :expected-result :failed
  (should (equal
           (s-collapse-whitespace (org-export-string-as "bible:Genesis:1:2" 'html :body-only))

"<p> <span style=\"color:nil;font-size:nil;\"> ﴾<em> <span class=\"verse-num\"><strong></strong>&nbsp;Now the earth was formless and empty, darkness was over the surface of the deep, and the Spirit of God was hovering over the waters. </em>﴿ <small><a href=\"https://www.christianity.com/bible/bible.php?q=Genesis+1&ver=niv\">Genesis 1:2</a></small> </span></p> ")))
