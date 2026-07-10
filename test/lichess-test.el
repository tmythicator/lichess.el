;;; lichess-test.el --- Tests for lichess.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-util)
(require 'lichess-fen)
(require 'lichess-game)
(require 'lichess-board-tui)
(require 'lichess-board-gui)
(require 'lichess)

;;; lichess-util.el tests

(ert-deftest lichess-util-aget-test ()
  "Test `lichess-util--aget` with alists and hash tables."
  ;; Alist tests
  (let ((al '((name . "Alice") ("title" . "GM") (rating . 2500))))
    (should (equal (lichess-util--aget al 'name) "Alice"))
    (should (equal (lichess-util--aget al "name") "Alice"))
    (should (equal (lichess-util--aget al 'title) "GM"))
    (should (equal (lichess-util--aget al "title") "GM"))
    (should (equal (lichess-util--aget al 'rating) 2500))
    (should (equal (lichess-util--aget al 'nonexistent) nil)))

  ;; Hash table tests
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "name" "Bob" ht)
    (puthash 'title "IM" ht)
    (should (equal (lichess-util--aget ht 'name) "Bob"))
    (should (equal (lichess-util--aget ht "name") "Bob"))
    (should (equal (lichess-util--aget ht 'title) "IM"))
    (should (equal (lichess-util--aget ht "title") "IM"))
    (should (equal (lichess-util--aget ht 'nonexistent) nil)))

  ;; Regression test for mixed key types (crash with string=)
  (let ((mixed-alist '((123 . "number-key") ("str" . "string-key"))))
    (should (equal (lichess-util--aget mixed-alist "str") "string-key"))
    (should (equal (lichess-util--aget mixed-alist 'str) "string-key"))))

(ert-deftest lichess-util-game-vs-test ()
  "Test `lichess-util--game->vs`."
  (let ((game '((players . ((white . ((user . ((name . "Alice") (title . "GM"))) (rating . 2500)))
                            (black . ((userId . "bob") (rating . 1500))))))))
    (should (equal (lichess-util--game->vs game) "GM Alice (2500)  vs  bob (1500)"))))

(ert-deftest lichess-face-definition-test ()
  "Test that `lichess-core-board-face' is defined and inherits from `fixed-pitch'."
  (should (facep 'lichess-core-board-face))
  (let ((inherit (face-attribute 'lichess-core-board-face :inherit)))
    (should (if (listp inherit)
                (memq 'fixed-pitch inherit)
              (eq 'fixed-pitch inherit)))))

(ert-deftest lichess-game-render-face-test ()
  "Verify that `lichess-game-render` applies `lichess-core-board-face`."
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (lichess-board-tui-preferred-style "ascii"))
    (with-temp-buffer
      ;; Setup mock state
      (setq-local lichess-game--state
                  (lichess-game-create
                   :fen-history (vector fen)
                   :current-idx 0
                   :perspective 'white
                   :eval-cache (make-hash-table)))
      (lichess-game-render)
      (goto-char (point-min))
      ;; Search for the board start (first pipe)
      (search-forward "|")
      (backward-char)
      ;; Verify face at this point
      (let ((face (get-text-property (point) 'face)))
        (should (eq face 'lichess-core-board-face))))))

;;; Dispatch tests

(ert-deftest lichess-board-dispatch-test ()
  "Test that `lichess-board-draw` dispatches correctly."
  (let ((pos (lichess-fen-pos-create)))
    ;; Case 1: Style "svg", GUI available -> calls GUI
    (cl-letf (((symbol-function 'lichess-board-gui-available-p) (lambda () t))
              ((symbol-function 'lichess-board-gui-draw) (lambda (_ _ _ &optional _) "GUI"))
              ((symbol-function 'lichess-board-tui-draw) (lambda (_ _ _) "TUI")))
      (let ((lichess-board-gui-preferred-style "svg"))
        (should (equal (lichess-board-draw pos) "GUI"))))

    ;; Case 2: Style "svg", GUI NOT available -> Fallback to TUI (Unicode)
    (cl-letf (((symbol-function 'lichess-board-gui-available-p) (lambda () nil))
              ((symbol-function 'lichess-board-gui-draw) (lambda (_ _ _ &optional _) "GUI"))
              ((symbol-function 'lichess-board-tui-draw) (lambda (_ _ _) "TUI")))
      (let ((lichess-board-gui-preferred-style "svg")
            (lichess-board-tui-preferred-style "unicode"))
        (should (equal (lichess-board-draw pos) "TUI"))))

    ;; Case 3: Style "ascii" -> calls TUI
    (cl-letf (((symbol-function 'lichess-board-gui-available-p) (lambda () t))
              ((symbol-function 'lichess-board-gui-draw) (lambda (_ _ _ &optional _) "GUI"))
              ((symbol-function 'lichess-board-tui-draw) (lambda (_ _ _) "TUI")))
      (let ((lichess-board-tui-preferred-style "ascii")
            (lichess-board-gui-preferred-style "ascii"))
        (should (equal (lichess-board-draw pos) "TUI"))))))

(ert-deftest lichess-set-style-test ()
  "Test `lichess-set-style` interactivity and warnings."
  (let ((warnings '())
        (custom-set-val nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_ msg &rest _) (push msg warnings)))
              ((symbol-function 'customize-set-variable)
               (lambda (_ val) (setq custom-set-val val)))
              ((symbol-function 'message) #'ignore))

      ;; 1. Success case (ASCII)
      (setq warnings '()) 
      (lichess-set-style "ascii")
      (should (string= custom-set-val "ascii"))
      (should (null warnings))

      ;; 2. Warning: Terminal mode
      (setq warnings '())
      (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
        (lichess-set-style "svg"))
      (should (string-match "Emacs is not running in graphical mode" (car warnings)))
      (should (string= custom-set-val "svg")) ;; It still sets it

      ;; 3. Warning: No SVG support
      (setq warnings '())
      (cl-letf (((symbol-function 'display-graphic-p) (lambda () t))
                ((symbol-function 'lichess-board-gui-available-p) (lambda () nil)))
        (lichess-set-style "svg"))
      (should (string-match "SVG support is missing" (car warnings)))

      ;; 4. Warning: Missing Assets
      (setq warnings '())
      (cl-letf (((symbol-function 'display-graphic-p) (lambda () t))
                ((symbol-function 'lichess-board-gui-available-p) (lambda () t))
                ((symbol-function 'lichess-board-gui-missing-assets) (lambda () '("wK.svg"))))
        (lichess-set-style "svg"))
      (should (string-match "assets are missing: wK.svg" (car warnings))))))

(ert-deftest lichess-token-resolution-test ()
  "Test that `lichess-token` resolves correctly using the variable and auth-source."
  (let ((lichess-token nil))
    ;; 1. If lichess-token is nil and auth-source returns nil, it should be nil
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil)))
      (should-not (lichess-token)))

    ;; 2. If lichess-token variable is set, it should return that variable
    (let ((lichess-token "custom-token-val"))
      (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) (error "Should not be called"))))
        (should (string= (lichess-token) "custom-token-val"))))

    ;; 3. If lichess-token variable is nil, it should look up via auth-source
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (should (equal (plist-get args :host) "lichess.org"))
                 (should (equal (plist-get args :require) '(:secret)))
                 '((:host "lichess.org" :secret "auth-source-token-val")))))
      (should (string= (lichess-token) "auth-source-token-val")))

    ;; 4. If auth-source returns a function for secret, it should execute it
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args)
                 '((:host "lichess.org" :secret (lambda () "auth-source-func-token-val"))))))
      (should (string= (lichess-token) "auth-source-func-token-val")))))

(provide 'lichess-test)
;;; lichess-test.el ends here
