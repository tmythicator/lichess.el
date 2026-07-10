;;; lichess-fen-test.el --- Tests for lichess-fen.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-fen)
(require 'lichess-board-tui)

;;; Code:

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
    (should (eq (plist-get pos :stm) 'w))
    (should (equal (plist-get pos :castle) "KQkq"))
    (should (equal (plist-get pos :ep) nil))
    (should (= (plist-get pos :halfmove) 0))
    (should (= (plist-get pos :fullmove) 1))
    ;; Check corner pieces
    (should (= (aref (aref (plist-get pos :board) 0) 0) ?r))
    (should (= (aref (aref (plist-get pos :board) 7) 7) ?R)))

  (let* ((fen "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3")
         (pos (lichess-fen-parse fen)))
    (should (eq (plist-get pos :stm) 'w))
    (should (= (plist-get pos :halfmove) 2))
    (should (= (plist-get pos :fullmove) 3))
    ;; Check piece at e4
    (should (= (aref (aref (plist-get pos :board) 4) 4) ?P))))

(ert-deftest lichess-fen--stm-test ()
  "Test `lichess-fen--stm` extracts side-to-move correctly and fast."
  (should (eq (lichess-fen--stm "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") 'w))
  (should (eq (lichess-fen--stm "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1") 'b))
  (should (eq (lichess-fen--stm "startpos") 'w))
  (should (eq (lichess-fen--stm nil) nil)))

(ert-deftest lichess-fen--fullmove-test ()
  "Test `lichess-fen--fullmove` extracts the fullmove number correctly."
  (should (= (lichess-fen--fullmove "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") 1))
  (should (= (lichess-fen--fullmove "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 5") 5))
  (should (= (lichess-fen--fullmove "startpos") 1))
  (should (= (lichess-fen--fullmove nil) 1)))

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
    (plist-put pos :eval "0.0")
    (let ((render (lichess-board-tui-draw pos "ascii" 'white)))
      ;; Check if "Eval" header and evaluation blocks are present
      (should (string-match "Eval" render))
      (should (string-match "░" render))
      (should (string-match "█" render)))))

(provide 'lichess-fen-test)
;;; lichess-fen-test.el ends here
