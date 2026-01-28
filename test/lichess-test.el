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

;;; lichess-fen.el tests

(ert-deftest lichess-fen-parse-ep-test ()
  "Test `lichess-fen--parse-ep`."
  (should (equal (lichess-fen--parse-ep "-") nil))
  (should (equal (lichess-fen--parse-ep "e3") '(5 . 4)))
  (should (equal (lichess-fen--parse-ep "a6") '(2 . 0)))
  ;; Invalid en passant squares should signal user-error
  (should-error (lichess-fen--parse-ep "e4") :type 'user-error)
  (should-error (lichess-fen--parse-ep "z9") :type 'user-error))

(ert-deftest lichess-fen-parse-fen-test ()
  "Test `lichess-fen-parse`."
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (pos (lichess-fen-parse fen)))
    (should (eq (lichess-pos-stm pos) 'w))
    (should (equal (lichess-pos-castle pos) "KQkq"))
    (should (equal (lichess-pos-ep pos) nil))
    (should (= (lichess-pos-halfmove pos) 0))
    (should (= (lichess-pos-fullmove pos) 1))
    ;; Check corner pieces
    (should (= (aref (aref (lichess-pos-board pos) 0) 0) ?r))
    (should (= (aref (aref (lichess-pos-board pos) 7) 7) ?R)))

  (let* ((fen "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3")
         (pos (lichess-fen-parse fen)))
    (should (eq (lichess-pos-stm pos) 'w))
    (should (= (lichess-pos-halfmove pos) 2))
    (should (= (lichess-pos-fullmove pos) 3))
    ;; Check piece at e4
    (should (= (aref (aref (lichess-pos-board pos) 4) 4) ?P))))

(ert-deftest lichess-fen-rows-to-board-test ()
  "Test `lichess-fen--rows->board`."
  (let* ((rows '("rnbqkbnr" "pppppppp" "8" "8" "8" "8" "PPPPPPPP" "RNBQKBNR"))
         (board (lichess-fen--rows->board rows)))
    (should (= (length board) 8))
    (should (= (length (aref board 0)) 8))
    (should (= (aref (aref board 0) 0) ?r))
    (should (= (aref (aref board 2) 0) ?.)))

  ;; Error cases
  (should-error (lichess-fen--rows->board '("rnbqkbnr" "pppppppp")) :type 'user-error) ; Too few rows
  (should-error (lichess-fen--rows->board '("rnbqkbnr" "pppppppp" "8" "8" "8" "8" "PPPPPPPP" "RNBQKBNR2")) :type 'user-error)) ; Row overflow

(ert-deftest lichess-fen-piece-to-unicode-test ()
  "Test `lichess-board-tui--piece->unicode`."
  (should (equal (lichess-board-tui--piece->unicode ?K) "♔"))
  (should (equal (lichess-board-tui--piece->unicode ?k) "♚"))
  (should (equal (lichess-board-tui--piece->unicode ?.) "·")))

(defun lichess-test--verify-alignment (rendered)
  "Verify that all separators '|' in RENDERED are vertically aligned.
Skips lines with fewer than 2 separators (like the ASCII separator line)."
  (with-temp-buffer
    (insert rendered)
    (goto-char (point-min))
    (let (separator-columns)
      (while (not (eobp))
        (let ((line-separators '())
              (line-start (line-beginning-position)))
          (save-excursion
            (while (search-forward "|" (line-end-position) t)
              (push (- (point) line-start 1) line-separators)))
          (setq line-separators (nreverse line-separators))
          ;; Only compare lines with at least 2 pipes (the board rows and header)
          (when (> (length line-separators) 1)
            (if separator-columns
                (should (equal separator-columns line-separators))
              (setq separator-columns line-separators))))
        (forward-line 1)))))

(ert-deftest lichess-fen-render-board-test ()
  "Test `lichess-board-tui-draw` output and alignment."
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (pos (lichess-fen-parse fen))
         (render (lichess-board-tui-draw pos "ascii" 'white))
         (expected (concat "|r|n|b|q|k|b|n|r|8\n"
                           "|p|p|p|p|p|p|p|p|7\n"
                           "|.|.|.|.|.|.|.|.|6\n"
                           "|.|.|.|.|.|.|.|.|5\n"
                           "|.|.|.|.|.|.|.|.|4\n"
                           "|.|.|.|.|.|.|.|.|3\n"
                           "|P|P|P|P|P|P|P|P|2\n"
                           "|R|N|B|Q|K|B|N|R|1\n"
                           "|-+-+-+-+-+-+-+-+-\n"
                           "|a|b|c|d|e|f|g|h| ")))
    (lichess-test--verify-alignment render)
    (should (equal render expected))
    ;; Check specific ranks for alignment and content
    (should (string-match "|r|n|b|q|k|b|n|r|8" render))
    (should (string-match "|\\.\\|\\.\\|\\.\\|\\.\\|\\.\\|\\.\\|\\.\\|\\.\\|5" render))
    (should (string-match "|R|N|B|Q|K|B|N|R|1" render))))

(ert-deftest lichess-fen-render-board-unicode-test ()
  "Test `lichess-board-tui-draw` with Unicode pieces and alignment."
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (pos (lichess-fen-parse fen))
         (render (lichess-board-tui-draw pos "unicode" 'white))
         (expected (concat "|♜|♞|♝|♛|♚|♝|♞|♜|8\n"
                           "|♟|♟|♟|♟|♟|♟|♟|♟|7\n"
                           "|·|·|·|·|·|·|·|·|6\n"
                           "|·|·|·|·|·|·|·|·|5\n"
                           "|·|·|·|·|·|·|·|·|4\n"
                           "|·|·|·|·|·|·|·|·|3\n"
                           "|♙|♙|♙|♙|♙|♙|♙|♙|2\n"
                           "|♖|♘|♗|♕|♔|♗|♘|♖|1\n"
                           "|-+-+-+-+-+-+-+-+-\n"
                           "|a|b|c|d|e|f|g|h| ")))
    (lichess-test--verify-alignment render)
    (should (equal render expected))
    ;; Check specific ranks for alignment and content
    (should (string-match "|♜|♞|♝|♛|♚|♝|♞|♜|8" render))
    (should (string-match "|·|·|·|·|·|·|·|·|5" render))
    (should (string-match "|♖|♘|♗|♕|♔|♗|♘|♖|1" render))))

(ert-deftest lichess-fen-render-board-eval-test ()
  "Test `lichess-board-tui-draw` with evaluation bar."
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (pos (lichess-fen-parse fen)))
    (setf (lichess-pos-eval pos) "0.0")
    (let ((render (lichess-board-tui-draw pos "ascii" 'white)))
      ;; Check if "Eval" header and evaluation blocks are present
      (should (string-match "Eval" render))
      (should (string-match "░" render))
      (should (string-match "█" render)))))

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
                  (make-lichess-game
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
  (let ((pos (make-lichess-pos)))
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

(provide 'lichess-test)
;;; lichess-test.el ends here
