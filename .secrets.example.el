;; Copy this file to .secrets.el and replace "your-token" with your actual Lichess API token.
;; Make sure .secrets.el is in your .gitignore (it is by default).

;; 1. Set your token
(setq lichess-token "your-token")

;; 2. Setup dev environment (add current directory to load-path)
(add-to-list 'load-path default-directory)
(require 'lichess)

;; 3. Done! You can now use M-x lichess-tv or other commands.
(message "Lichess.el dev environment loaded!")