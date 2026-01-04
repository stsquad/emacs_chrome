# AGENTS.md

## Project Overview

"Edit with Emacs" is a browser extension for Firefox and Chrome-based browsers that allows users to edit text areas from web pages in their Emacs editor.

### Architecture
- **The Browser Extension**: JavaScript-based. It identifies editable fields, extracts text, and sends it to a local server.
- **The Edit Server (`edit-server.el`)**: An Emacs Lisp script running inside Emacs. It listens for HTTP requests (default port 9292), opens a buffer, and returns the modified text upon buffer kill/save.

The project is licensed under the GPL v3.

## Project Structure

- `/` (Root): Contains v2 `manifest.json`, `Makefile` and project-wide documentation and metadata.
- `/css/`: CSS for the browser extension.
- `/fancy-settings/`: a vendored copy of fancy-setting library
- `/html/`: the background.html for the browser extension
- `/icons/`: icons and indicators for the browser extension
- `/javascript/`: the javascript for the browser extension
- `/lib/`: mini-fied copies of supporting javascript libs
- `/servers/`: example servers including the main Emacs `edit-server.el`

This file contains instructions for AI agents to understand and interact with this project.

## Project Overview

"Edit with Emacs" is a browser extension for Firefox and other WebExtension-compatible browsers that allows users to edit text areas from web pages in their Emacs editor. It consists of two main components:

1.  **The Browser Extension**: Written in JavaScript, it identifies editable text areas on a web page and sends their contents to the Edit Server.
2.  **The Edit Server**: An Emacs Lisp script (`edit-server.el`) that runs within Emacs, listening for requests from the browser extension. It opens a new Emacs buffer with the text from the web page, and when the user saves and closes the buffer, it sends the updated text back to the browser.

The project is licensed under the GPL v3.

## Key Technologies

-   **JavaScript**: For the browser extension logic.
-   **Emacs Lisp**: For the Edit Server.
-   **HTML/CSS**: For the extension's options page and UI elements.
-   **WebExtensions API**: The standard for browser extension development.

## Build & Test Info

- **Make**: Use `make` to see available targets.
- **Testing**:
    - JavaScript tests (if any) are located in `/test`.
    - To test the extension: Load the directory as a "Temporary Extension" in `about:debugging` (Firefox) or `chrome://extensions` (Chrome).
    - To test the server: `M-x load-file edit-server.el` followed by `M-x edit-server-start`.

## Agent Guidelines

- **Namespacing**: When writing JavaScript, prefer the `browser` namespace or a compatibility polyfill unless specific to one browser.
- **Elisp Style**: Follow standard Emacs Lisp naming conventions (e.g., prefix functions with `edit-server-`).
- **No Direct Commits**: Agents should suggest changes in code blocks or provide patch files. Do not attempt to commit directly to the repository.
- **Documentation**: If adding a feature to the extension, ensure the corresponding configuration option is added to the Options page and documented in the README.
- **Communication Protocol**: If modifying the data sent between the browser and Emacs, ensure backward compatibility so older versions of the extension can still talk to newer versions of the server script (and vice versa).
