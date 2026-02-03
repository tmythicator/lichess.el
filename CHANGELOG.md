# Changelog

All notable changes to this project will be documented in this file.

## [0.8] - 2026-02-03

### Added

- **Challenge Management**: New UI for listing, accepting, and canceling challenges (`M-x lichess-challenge-list`).
- **Friend Challenges**: Improved flow for challenging friends, including offline friends and manual username entry.
- **Accessibility**: Added game event announcer for screen readers (`lichess-announce`).
- **Game Variants**: Ability to select game variants (Standard, Chess960, etc.) when sending a challenge.
- **Auto-Flip**: Board automatically flips to the player's perspective when a game starts (works for both Human and AI opponents).
- **Keybindings**: Added `a` (accept) and `g` (refresh) to the challenge list.

### Changed

- **Refactor**: Centralized game variant definitions in `lichess-core.el`.
- **Refactor**: Updated `lichess-debug` to reuse shared variant definitions.
- **Versioning**: Bumped project version to 0.8.

## [0.7] - 2026-01-28

### Added

- **Tournament Broadcasts**: New modules `lichess-broadcast-list.el` and `lichess-broadcast-view.el` for watching live tournament broadcasts in a grid layout.
- **API module**: Centralized all Lichess API calls into `lichess-api.el` for better maintainability and robustness.

### Fixed

- **Unicode**: Added proper UTF-8 decoding for HTTP responses to handle international names.
- **Robustness**: Fixed a crash in `lichess-util--aget` when handling mixed key types (symbols/strings/numbers).
- **Broadcast Fetching**: Implemented ultra-minimal URL-to-API-path detection for broadcast rounds.

### Changed

- **Code Quality**: Refactored broadcast views using Clojure-style functional patterns (pcase-let, sequence operations).
- **Namespace Alignment**: Renamed all broadcast-related symbols to follow package naming conventions.
- **Versioning**: Bumped project version to 0.7.

## [0.6] - 2026-01-27

### Added

- **Sidebar**: Now displays move history and chat in a dedicated sidebar.
- **Controls**: Interactive control buttons for game navigation and actions.
- **Material Diff**: Real-time material difference display.

### Changed

- **UI**: Significant UI updates for better aesthetics and usability.
- **Compatibility**: Fixed `string-search` issue for Emacs 27.1 compatibility.

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
