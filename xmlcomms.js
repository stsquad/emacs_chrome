/*
 * xmlcomms.js
 *
 * This handles making XMLHttp calls to the remote end
 *
 * This file is part of emacs_chrome (http://github.com/stsquad/emacs_chrome)
 * and licensed under the GPLv3. See the COPYING file for details
 */

// This is the edit server address
var urlPrefix = "http://127.0.0.1:9292/";

/*
 * Give some feedback to the user via the icon/hover text.
 */
function updateUserFeedback(string, redIcon)
{
    console.log("updateUserFeedback: "+string);
    chrome.browserAction.setTitle({title:string});
    if (redIcon) {
	chrome.browserAction.setIcon({path:"emacs23-16x16-red.png"});
    } else {
    	chrome.browserAction.setIcon({path:"emacs23-16x16.png"});
    }
}
    
// Initial message
updateUserFeedback("Awaiting edit request", false);

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
  updateUserFeedback("sent request to content script", false);
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
    var url = urlPrefix + cmd + "/";
    url = url + id;

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

		updateUserFeedback("Last Edit request a success", false);
		tab_port.postMessage(update_msg);
	    } else if (xhr.status == 0) {
		// Is the edit server actually running?
		updateUserFeedback("Error: is edit server running?", true);
	    } else {
		updateUserFeedback("Un-handled response: "+xhr.status, true); 
	    }
        }
    }

    // reset the display before sending request..
    updateUserFeedback("Edit request sent", false);

    xhr.setRequestHeader("Content-type", "text/plain");
    xhr.send(text);
}

function contentTalking(port)
{
    port.onMessage.addListener(function(msg, port) {
	handleContentMessages(msg,port);
    });
}

// Hook up whenever someone connects to the extension comms port
chrome.extension.onConnect.addListener(contentTalking);
