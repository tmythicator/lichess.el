# Contributing to Lichess.el

First off, thank you for considering contributing to **Lichess.el**! It's people like you that make this tool better for everyone. Whether you're fixing a bug, adding a new feature, or improving documentation, your help is welcome.

## Welcome

We love pull requests from everyone. You don't need to be an Emacs wizard to contribute — if you're stuck, just ask!

## Getting Started

### Prerequisites

- **Emacs**: 27.1 or later.
- **Nix** (Optional but recommended): We provide a reproducible development environment via Nix.

### Setting up the environment

1. **Clone the repository**:

   ```bash
   git clone https://github.com/tmythicator/lichess.el.git
   cd lichess.el
   ```

2. **Load dependencies**:

   **With Nix (Recommended):**
   If you use `direnv`, just run:

   ```bash
   direnv allow
   ```

   Otherwise, enter the shell manually:

   ```bash
   nix develop
   ```

   **Without Nix:**
   You will need to install the following Emacs packages manually if you want to run tests and linters locally:
   - `elisp-autofmt`
   - `package-lint`

## Easy Start

Want to start hacking immediately? Here is the fastest way to get your environment running.

1.  **Configure your token**:
    Copy `.secrets.example.el` to `.secrets.el` and add your [Lichess API Token](https://lichess.org/account/oauth/token).

    ```bash
    cp .secrets.example.el .secrets.el
    # Edit .secrets.el and paste your token
    ```

2.  **Load the project**:
    Open `.secrets.el` in Emacs and evaluate the buffer:
    `M-x eval-buffer`

    **Pro Tip**: Check out `.dev.el` for a pre-made scratchpad with common commands!

3.  **Run it!**:
    `M-x lichess-tv` - Watch Lichess TV.
    `M-x lichess-ai-challenge` - Play against Stockfish.

## Interactive Development

Emacs is a live environment. You don't need to restart it to see your changes!

- **Eval as you go**: Place your cursor inside a function you just modified and press `C-M-x` (or `M-x eval-defun`). The change takes effect immediately.
- **REPL**: Use `M-x ielm` to experiment with functions in an interactive shell.
- **Scratch Buffer**: The `*scratch*` buffer is perfect for testing small snippets of code before adding them to the project.

## Testing & Debugging

### Running Tests

- **Run all tests**: `make test` (runs in batch mode, good for CI check).
- **Run specific test**: Open a test file (e.g., `test/lichess-game-test.el`) and run `M-x ert`. You can then select specific tests to run interactively.

### Debugging

If something goes wrong, you can step through the code:

1.  Go to the function you want to debug.
2.  Press `C-u C-M-x` (`edebug-defun`) to instrument it.
3.  Run the command that calls this function.
4.  Emacs will pause execution. Use `SPC` to step through, `i` to step into functions, and `q` to quit.

### Debugging a Live Game

If you are watching a game and want to inspect its state:

1.  **Open IELM**: `M-x ielm`.
2.  **Switch Context**: Press `C-c C-b` and select the game buffer (e.g., `*Lichess Game Stream: ...*`).
    Now your IELM shell evaluates expressions _as if_ it were in that buffer.
3.  **Inspect State**: The main state variable is `lichess-game--state`.

    ```elisp
    lichess-game--state
    ;; => #s(lichess-game ...)

    (lichess-game-fen-history lichess-game--state)
    ;; => ["rnbqk..." ...]
    ```

[[file:demo/lichess-dev-example.png]]

### Clean Environment

To ensure your personal config isn't interfering, you can run Emacs with a clean slate:

```bash
# Run with only lichess.el loaded
emacs -Q -L . -l lichess.el
```

## Development Cycle

We use a `Makefile` to make development easy. Here are the most important commands:

### 1. Formatting

We use `elisp-autofmt` to keep the codebase clean and consistent. Please format your code before submitting.

```bash
make format
```

### 2. Linting

Ensure your code follows packaging guidelines and common best practices.

```bash
make lint
```

### 3. Documentation Check

All exported functions and variables should have docstrings.

```bash
make checkdoc
```

### 4. Running Tests

Run the test suite to ensure no regressions.

```bash
make test
```

## Style Guide

- **Naming**: Use the `lichess-` prefix for all global symbols (functions, variables, faces).
- **Docstrings**: Write clear, concise docstrings. The first line should be a complete sentence.
- **Conventions**: Follow standard [Emacs Lisp conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html).

## Submitting a Pull Request

1. **Fork** the repo on GitHub.
2. **Clone** the project to your own machine.
3. **Commit** your changes to your own branch.
   - Use clear commit messages (e.g., `feat: serve captured pieces in UI` or `fix: handle network timeout`).
4. **Push** your work back to your fork.
5. **Submit** a Pull Request.

**Note**: All PRs are automatically checked for formatting, linting, and tests via GitHub Actions.

## Getting Help

If you have questions or just want to discuss an idea, feel free to open a [Discussion](https://github.com/tmythicator/lichess.el/discussions) or an [Issue](https://github.com/tmythicator/lichess.el/issues).

Happy Hacking! ♟️
