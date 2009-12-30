/*
 * xmlcomms.js
 *
 * This handles making XMLHttp calls to the remote end
 *
 */

// This is the edit server address
var urlPrefix = "http://127.0.0.1:9292/"
 
// Called when the user clicks on the browser action.
// When clicked send a message to the current active port:
chrome.browserAction.onClicked.addListener(function(tab) {
  console.log("Edit button clicked: "+JSON.stringify(tab));
  chrome.tabs.sendRequest(tab.id, {msg: "find_edit"});
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

		chrome.browserAction.setTitle({title:"Last Edit request a success"});
		tab_port.postMessage(update_msg);
	    } else if (xhr.status == 0) {
		// Is the edit server actually running?
		console.log("failed to spawn editor");
		chrome.browserAction.setTitle({title:"Error: is edit server running?"});
	    } else {
		console.log("Un-handled response: "+xhr.status); 
	    }
        }
    }
    
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
