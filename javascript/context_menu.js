// -*- tab-width:2; indent-tabs-mode:t -*- vim: set noet ts=2:
(function(){

	var edit_msg = {};
	var page_msg = {};

	function menuClicked(info, tab) {
		if (edit_msg) {
			var tab_port = chrome.tabs.connect(tab.id);
			tab_port.sender = { tab: tab };
			handleContentMessages(edit_msg, tab_port);
		}
	}


		function viewPageSource(info, tab) {
				var tab_port = chrome.tabs.connect(tab.id);
				tab_port.sender = { tab: tab };
				handleContentMessages(page_msg, tab_port);
		}

		// Set up context menu tree at install time.
		chrome.runtime.onInstalled.addListener(function() {
				// Create one test item for each context type.
				chrome.contextMenus.create({
						title: "View source with Emacs",
						contexts: ["page"],
						onclick: function(info, tab) {
								viewPageSource(info, tab);
						}
				});
		});

	var menu_enabled = false;

	function enableContextMenu() {
		if (!menu_enabled) {
			chrome.contextMenus.removeAll();
			chrome.contextMenus.create({
				title: "Edit with Emacs",
				contexts: ["editable"],
				onclick: function(info, tab) {
					menuClicked(info, tab);
				}
			});
			menu_enabled = true;
		}
	}

	function disableContextMenu() {
		chrome.contextMenus.removeAll();
		menu_enabled = false;
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
		} else if (request.type === "page_source") {
				page_msg = request.edit_msg;
		}	else if (request.type === "enable_contextmenu") {
			if (request.enabled) {
				enableContextMenu();
			} else {
				disableContextMenu();
			}
		}
		sendResponse({});
	}

	chrome.extension.onRequest.addListener(processRequest);
})();
