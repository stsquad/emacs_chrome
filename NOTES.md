Developer Notes
===============

Keyboard Shortcuts
------------------

Currently Firefox and Chrome both interperet the manifest.json
"commands" section differently. Chrome (at least on ChromeOS) doesn't
need an explicit "edit-textbox" command to allow configuring of the
trigger keyboard shortcut (which fires the
chrome.browserAction.onClicked action). However without this Firefox
doesn't have an easy way to configure the shortcut. The explicit
commands go via chrome.commands.onCommand. Both of these functions are
in xmlcomms.js
