(setq needed-libraries
      '(s cl-lib dash org))

(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg needed-libraries)
  (unless (package-installed-p pkg)
    (package-install pkg)))


(require 's)               ;; “The long lost Emacs string manipulation library”
(require 'dash)            ;; “A modern list library for Emacs”
(require 'cl-lib)          ;; New Common Lisp library; ‘cl-???’ forms.
(require 'org)

(load-file "holy-books.el")


(ert-deftest basmala/empty ()
  (should (equal
           (org-export-string-as "[[basmala:]]" 'html :body-only)
"<p>
<center style=\"color:;font-size:60px;padding:25px\">
                     ﷽
                </center></p>
")))
