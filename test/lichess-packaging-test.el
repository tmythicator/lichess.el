;;; lichess-packaging-test.el --- Tests for package distribution logic -*- lexical-binding: t; -*-

(require 'ert)
(require 'lichess)
(require 'cl-lib)

(ert-deftest lichess-no-debug-library-test ()
  "Ensure `lichess' command works and hides debug options when lichess-debug is missing."
  (cl-letf (((symbol-function 'locate-library) (lambda (_lib) nil))
            ((symbol-function 'completing-read)
             (lambda (_prompt collection &rest _args)
               ;; Just return a dummy valid value to avoid erroring,
               ;; but we strictly want to inspect 'collection'
               (let ((keys collection))
                 ;; Assert that NO debug commands are present
                 (should-not (seq-some (lambda (k) (string-match-p "(Debug)" k)) keys))
                 ;; Assert that normal commands ARE present
                 (should (member "Lichess TV: Show channels" keys))
                 "Lichess TV: Show channels"))))
    ;; Run the function; it should not error.
    (lichess)))

(provide 'lichess-packaging-test)
;;; lichess-packaging-test.el ends here
