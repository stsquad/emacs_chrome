/* -*- tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
(function(){

    var edit_msg = null;

    function menuClicked(info, tab) {
        if (edit_msg) {
            var tab_port = chrome.tabs.connect(tab.id);
            edit_msg.pageUrl = info.pageUrl;
            handleContentMessages(edit_msg, tab_port);
            edit_msg = null;
        } else {
            console.error("menuClicked called while edit_msg is null");
        }
    }

	var menu_is_installed = false;

	function enableContextMenu() {
		if (!menu_is_installed) {
			chrome.contextMenus.removeAll();
			chrome.contextMenus.create({
				title: "Edit with Emacs",
				contexts: ["editable"],
				onclick: function(info, tab) {
					menuClicked(info, tab);
				}
			});
			menu_is_installed = true;
		}
	}

	function disableContextMenu() {
		chrome.contextMenus.removeAll();
		menu_is_installed = false;
	}

	// Initialize the context menu based on stored options.
	// Also, default to enabled if the setting hasn't been saved before.
	if (localStorage.enable_contextmenu === "true" ||
			!localStorage.hasOwnProperty('enable_contextmenu')) {
		enableContextMenu();
	} else {
		disableContextMenu();
	}


	function processRequest(request, sender, sendResponse) {
		if (request.type === "menu_target") {
			edit_msg = request.edit_msg;
		} else if (request.type === "enable_contextmenu") {
			if (request.enabled) {
				enableContextMenu();
			} else {
				disableContextMenu();
			}
		}
		sendResponse({});
	}

    // Check for Firefox compatibility
    if (typeof browser !== 'undefined') {
        browser.runtime.onMessage.addListener(processRequest);
    } else {
        chrome.extension.onRequest.addListener(processRequest);
    }

})();
