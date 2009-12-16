/*
 * xmlcomms.js
 *
 * This handles making XMLHttp calls to the remote end
 *
 */

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
function handleContentMessages(port, msg)
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
	    port.postMessage({id: id, text: xhr.responseText});
        }
    }
    
    xhr.setRequestHeader("Content-type", "text/plain");
    xhr.send(text);
}

function contentTalking(port)
{
    //    console.log("contentTalking:"+port);
    port.onMessage.addListener(port, handleContentMessages);
}

// Hook up whenever someone connects to the extension comms port
chrome.extension.onConnect.addListener(contentTalking);
