/*
 * xmlcomms.js
 *
 * This handles making XMLHttp calls to the remote end
 *
 */

var reply_port = null;

// This is the edit server address
var urlPrefix = "http://127.0.0.1:9292/"
 
// Called when the user clicks on the browser action.
chrome.browserAction.onClicked.addListener(function(tab) {
  console.log("Thingy clicked!");
  chrome.tabs.executeScript(tab.id, {file: "textareas.js", allFrames: false});
  console.log("Run update");
});

// Handle and edit request coming from the content page script
//
// Package up the text to be edited and send it to the edit server
function handleContentMessages(msg)
{
    console.log("handleContentMessages called:"+msg);
    var cmd = msg.cmd;
    var id = msg.id;
    var text = msg.text;

    var xhr = new XMLHttpRequest();
    var url = urlPrefix + cmd + "/";
    url = url + id;

    console.log(" request URL:"+url);
    
    xhr.open("POST", url, true);
    
    xhr.onreadystatechange = function() {
        console.log("State change!");
        if(xhr.readyState == 4 && xhr.status == 200) {

	    var update_msg = {
		msg: "update",
		text: xhr.responseText,
		id: id
	    };

	    //port.postMessage(tab.id, update_msg);
	    reply_port.postMessage(update_msg);
        }
    }
    
    xhr.setRequestHeader("Content-type", "text/plain");
    xhr.send(text);
}

function contentTalking(port)
{
    console.log("contentTalking:"+port);
    reply_port = port;// will this cause a problem with multiple tabs connecting at once?
    port.onMessage.addListener(handleContentMessages);
}

// Hook up whenever someone connects to the extension comms port
chrome.extension.onConnect.addListener(contentTalking);
