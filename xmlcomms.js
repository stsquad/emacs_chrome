/*
 * xmlcomms.js
 *
 * This handles making XMLHttp calls to the "Edit Server"
 *
 * As no other parts of the extension can make xhr requests this
 * is essentially the gatekeeper to all contact with the Edit
 * Server.
 *
 * This file is part of emacs_chrome (http://github.com/stsquad/emacs_chrome)
 * and licensed under the GPLv3. See the COPYING file for details
 */

// Get the base URL from which we make all requests to the server..
function getEditUrl()
{
    var port = localStorage["edit_server_port"];
    if (!port) {
	port = 9292;
    }
    return "http://127.0.0.1:" +  port + "/";
}

/*
 * Give some feedback to the user via the icon/hover text.
 */
function updateUserFeedback(string, colour)
{
    console.log("updateUserFeedback: "+string);
    chrome.browserAction.setTitle({title:string});
    if (colour == null) {
    	chrome.browserAction.setIcon({path:"emacs23-16x16.png"});
    } else if (colour == "green") {
	chrome.browserAction.setIcon({path:"emacs23-16x16-green.png"});
    } else if (colour == "red") {
	chrome.browserAction.setIcon({path:"emacs23-16x16-red.png"});
    } else {
    	chrome.browserAction.setIcon({path:"emacs23-16x16.png"});
    }
}
    
// Initial message
updateUserFeedback("Awaiting edit request", "blue");

// Called when the user clicks on the browser action.
//    
// When clicked we send a message to the current active tab's
// content script. It will then use heuristics to decide which text
// area to spawn an edit request for.
chrome.browserAction.onClicked.addListener(function(tab) {
  
  var find_msg = {
      msg: "find_edit"
  };
  var tab_port = chrome.tabs.connect(tab.id);
  
  tab_port.postMessage(find_msg);
  updateUserFeedback("sent request to content script", "green");
});

// Handle and edit request coming from the content page script
//
// Package up the text to be edited and send it to the edit server
function handleContentMessages(msg, tab_port)
{
    console.log("handleContentMessages called:"+JSON.stringify(msg));
    var cmd = msg.msg;
    var id = msg.id;
    var text = msg.text;

    var xhr = new XMLHttpRequest();
    var url = getEditUrl() + cmd;

    console.log(" page URL:"+tab_port.tab.url);
    console.log(" tab_port:"+tab_port.portId_);
    console.log(" request URL:"+url);
    
    xhr.open("POST", url, true);
    
    xhr.onreadystatechange = function() {
        console.log("State change:"+ xhr.readyState + " status:"+xhr.status);
	// readyState 4=HTTP response complete
        if(xhr.readyState == 4) {
	    if (xhr.status == 200) {
		
		var update_msg = {
		    msg: "update",
		    text: xhr.responseText,
		    id: id
		};

		updateUserFeedback("Successful edit of "+msg.title);
		tab_port.postMessage(update_msg);
	    } else if (xhr.status == 0) {
		// Is the edit server actually running?
		updateUserFeedback("Error: is edit server running?", "red");
	    } else {
		updateUserFeedback("Un-handled response: "+xhr.status, "red"); 
	    }
        }
    }

    // reset the display before sending request..
    updateUserFeedback("Edit request sent for "+msg.title, "green");

    xhr.setRequestHeader("Content-type", "text/plain");
    xhr.setRequestHeader("x-url", tab_port.tab.url);
    xhr.setRequestHeader("x-id", id);
    xhr.send(text);
}

// Handle and edit request coming from the content page script
//
// Package up the text to be edited and send it to the edit server
function handleTestMessages(msg, tab_port)
{
    var url = getEditUrl() + "status";
    var xhr = new XMLHttpRequest();
    xhr.open("GET", url, true);
    xhr.onreadystatechange = function() {
	console.log("State change:"+ xhr.readyState + " status:"+xhr.status);
	// readyState 4=HTTP response complete
	if(xhr.readyState == 4) {
	    if (xhr.status == 200) {
		tab_port.postMessage({msg: "test_result", text: xhr.responseText});
	    } else if (xhr.status == 0) {
		tab_port.postMessage({msg: "test_result", text: "Edit Server Test failed: is it running?"});
	    } else {
		tab_port.postMessage({msg: "test_result", text: "Un-handled response: "+xhr.status}); 
	    }
	}
    }
    xhr.send();
}

// Handle config request messages, the textarea.js content script being in it's own
// isolated sandbox has to be fed all this via the IPC mechanisms

function getBooleanConfig(config, defaultval) {
    return typeof localStorage[config] == "undefined" ? defaultval : localStorage[config] == "true";
}


function handleConfigMessages(msg, tab_port)
{
    var config_msg = {
	msg: "config",
	enable_button: getBooleanConfig("enable_button", true),
	enable_dblclick: getBooleanConfig("enable_dblclick", false),
	enable_keys: getBooleanConfig("enable_keys", false)
    };
    tab_port.postMessage(config_msg);
}


/*
  Handle all in-coming messages to the extension.

  As other parts of the extension cannot trigger XHR requests they all
  send message to the main part of the extension to service these requests.
*/
 
function localMessageHandler(port)
{
    port.onMessage.addListener(function(msg, port) {
        if (msg.msg == "config") {
	    handleConfigMessages(msg, port);
	} else if (msg.msg == "edit") {
	    handleContentMessages(msg, port);
	} else if (msg.msg == "test") {
	    handleTestMessages(msg, port);
	} else if (msg.msg == "error") {
	    updateUserFeedback(msg.text, "red");
	}
    });
}

// Hook up whenever someone connects to the extension comms port
chrome.extension.onConnect.addListener(localMessageHandler);
