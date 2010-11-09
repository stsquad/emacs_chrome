// -*- tab-width:2; indent-tabs-mode:t -*- vim: set noet ts=2:
/*
 * options.js
 *
 * This is the JavaScript for the extension options page
 *
 * This file is part of emacs_chrome (http://github.com/stsquad/emacs_chrome)
 * and licensed under the GPLv3. See the COPYING file for details
 */


// Snarfed from AdThwart, not sure why checkboxes are so trixy
function loadCheckbox(id, config, defaultval) {
	document.getElementById(id).checked = typeof localStorage[config] == "undefined" ? defaultval : localStorage[config] == "true";
}
 
function saveCheckbox(id, config) {
	localStorage[config] = document.getElementById(id).checked;
}

// Saves options to localStorage
function save_options() {
	localStorage["edit_server_port"] = document.getElementById("port").value;
	saveCheckbox("editbutton", "enable_button");
	saveCheckbox("contextmenu", "enable_contextmenu");
	saveCheckbox("dblclick", "enable_dblclick");
	saveCheckbox("keyshort", "enable_keys");

	chrome.extension.sendRequest({
		type: "enable_contextmenu",
		enabled: (localStorage.enable_contextmenu === "true")
	});

	// Update status to let user know options were saved.
	var status = document.getElementById("status");
	status.innerHTML = "Options Saved.";
	setTimeout(function() {
		status.innerHTML = "";
	}, 750);
}

// Restores select box state to saved value from localStorage.
function restore_options() {
	var port = localStorage["edit_server_port"];
	if (!port) {
		port = 9292;
	}
	document.getElementById("port").value = port;
	loadCheckbox("editbutton", "enable_button", true);
	loadCheckbox("contextmenu", "enable_contextmenu", true);
	loadCheckbox("dblclick", "enable_dblclick", false);
	loadCheckbox("keyshort", "enable_keys", false);
}

/* Message handling multiplexer */
function localMessageHandler(msg, port) {
	// What was the bidding?
	var cmd = msg.msg;
	if (cmd == "test_result") {
	var status = document.getElementById("server_status");
	status.innerHTML = msg.text;
	} else {
	console.log("localMessageHandler: un-handled message:"+cmd);
	}
}

// Test for the presence of an Edit Server
function test_server() {
	var port = chrome.extension.connect();
	var status = document.getElementById("server_status");
	status.innerHTML = "testing...";
	port.onMessage.addListener(localMessageHandler);
	port.postMessage({msg: "test"});
}

