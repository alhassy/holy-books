(load-file "holy-books.el")

(ert-deftest basmala/empty ()
  (should (equal
           (org-export-string-as "[[basmala:]]" 'html :body-only)
"<p>
<center style=\"color:;font-size:60px;padding:25px\">
                     ﷽
                </center></p>
")))
