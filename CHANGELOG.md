# Changelog

All notable changes to this project will be documented in this file.

## [0.5] - 2026-01-19

### Changed

- **Board Rendering**: Refactored internal rendering to encapsulate `eval` and `info` metadata within the `lichess-pos` struct.
- **Documentation**: Updated `lichess.el` file description and commentary.

## [0.4] - 2026-01-18

### Changed

- **Package Distribution**: `lichess-debug.el` is now excluded from standard installs, reducing clutter for users while keeping tools available for contributors.
- **Code Quality**: Applied sharp-quoting for function calls and improved internal time formatting to use `%T`.
- **Assets**: Explicitly defined asset inclusion in package recipes (MELPA).

## [0.3] - 2026-01-17

### Added

- **Mouse Support**: Users can now click on the board to select and move pieces (GUI only).
- **GUI Highlights**: Selected squares are highlighted with translucent overlays in SVG mode.
- **TUI/GUI Separation**: Refactored board rendering dispatch to cleanly separate text and graphical logic.

### Changed

- `lichess-board-draw` now dispatches to GUI or TUI based on capabilities and user preference.
- Updated documentation to reflect mouse support.

### Removed

- Removed experimental mouse support logic from TUI renderer to focus on GUI stability.

## [0.2] - 2025-12-20

### Added

- Initial stable release.
- Lichess AI challenges.
- Lichess TV support.
- Game watcher with history navigation.
