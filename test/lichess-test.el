;;; lichess-test.el --- Tests for lichess.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-util)
(require 'lichess-fen)

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
    (should (equal (lichess-util--aget ht 'nonexistent) nil))))

(ert-deftest lichess-util-fmt-player-test ()
  "Test `lichess-util--fmt-player`."
  (should (equal (lichess-util--fmt-player "Alice" "GM" 2500) "GM Alice (2500)"))
  (should (equal (lichess-util--fmt-player "Bob" nil 1500) "Bob (1500)"))
  (should (equal (lichess-util--fmt-player "Charlie" "IM" nil) "IM Charlie"))
  (should (equal (lichess-util--fmt-player "Dave" nil nil) "Dave"))
  (should (equal (lichess-util--fmt-player nil nil nil) "Anonymous")))

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
  "Test `lichess-chess-parse-fen`."
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (pos (lichess-chess-parse-fen fen)))
    (should (eq (lichess-pos-stm pos) 'w))
    (should (equal (lichess-pos-castle pos) "KQkq"))
    (should (equal (lichess-pos-ep pos) nil))
    (should (= (lichess-pos-halfmove pos) 0))
    (should (= (lichess-pos-fullmove pos) 1))
    ;; Check corner pieces
    (should (= (aref (aref (lichess-pos-board pos) 0) 0) ?r))
    (should (= (aref (aref (lichess-pos-board pos) 7) 7) ?R)))
  
  (let* ((fen "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3")
         (pos (lichess-chess-parse-fen fen)))
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
  "Test `lichess-fen--piece->unicode`."
  (should (equal (lichess-fen--piece->unicode ?K) "♔"))
  (should (equal (lichess-fen--piece->unicode ?k) "♚"))
  (should (equal (lichess-fen--piece->unicode ?.) ".")))

(ert-deftest lichess-fen-render-org-table-test ()
  "Test `lichess-fen-render-org-table` output."
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (pos (lichess-chess-parse-fen fen))
         (expected-ascii (concat "|r|n|b|q|k|b|n|r|8|\n"
                                 "|p|p|p|p|p|p|p|p|7|\n"
                                 "|.|.|.|.|.|.|.|.|6|\n"
                                 "|.|.|.|.|.|.|.|.|5|\n"
                                 "|.|.|.|.|.|.|.|.|4|\n"
                                 "|.|.|.|.|.|.|.|.|3|\n"
                                 "|P|P|P|P|P|P|P|P|2|\n"
                                 "|R|N|B|Q|K|B|N|R|1|\n"
                                 "|-+-+-+-+-+-+-+-+-|\n"
                                 "|a|b|c|d|e|f|g|h| ")))
    (should (equal (lichess-fen-render-org-table pos nil 'white) expected-ascii))))

(ert-deftest lichess-fen-render-org-table-unicode-test ()
  "Test `lichess-fen-render-org-table` with Unicode pieces."
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (pos (lichess-chess-parse-fen fen))
         (expected-unicode (concat "|♜|♞|♝|♛|♚|♝|♞|♜|8|\n"
                                   "|♟|♟|♟|♟|♟|♟|♟|♟|7|\n"
                                   "|.|.|.|.|.|.|.|.|6|\n"
                                   "|.|.|.|.|.|.|.|.|5|\n"
                                   "|.|.|.|.|.|.|.|.|4|\n"
                                   "|.|.|.|.|.|.|.|.|3|\n"
                                   "|♙|♙|♙|♙|♙|♙|♙|♙|2|\n"
                                   "|♖|♘|♗|♕|♔|♗|♘|♖|1|\n"
                                   "|-+-+-+-+-+-+-+-+-|\n"
                                   "|a|b|c|d|e|f|g|h| ")))
    (should (equal (lichess-fen-render-org-table pos t 'white) expected-unicode))))

(ert-deftest lichess-fen-render-org-table-eval-test ()
  "Test `lichess-fen-render-org-table` with evaluation bar."
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
         (pos (lichess-chess-parse-fen fen))
         (render (lichess-fen-render-org-table pos nil 'white "0.0")))
    ;; Check if "Eval" header and evaluation blocks are present
    (should (string-match "Eval" render))
    (should (string-match "░" render))
    (should (string-match "█" render))))

(provide 'lichess-test)
;;; lichess-test.el ends here
