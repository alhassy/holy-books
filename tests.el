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


(load-file "holy-books.el")
