/*
 * TextAreas.js
 *
 * This "content" script finds TextArea's in the DOM and tags them
 * with a unique ID and edit button. When the button is
 * clicked it communicates with the master extension page to send an
 * edit request.
 *
 */
 
var editImgURL = chrome.extension.getURL("gumdrop.png");
var port = chrome.extension.connect();
var page_edit_id = 0;

console.log("textareas.js: port is "+JSON.stringify(port));

/*
 updateTextArea

 Called when we want to update the text area with our updated text
*/
function updateTextArea(id, content) {
    var texts = document.getElementsByTagName('textarea');
    for (var i=0; i<texts.length; i++) {
	var text = texts[i];

	var text_edit_id = text.getAttribute("edit_id");

	if (text_edit_id == id)
	{
	    text.value = content;
	}
    }
}

/*
  Find the current active text area and spawn an edit for it
*/
function findActiveTextArea() {
    var texts = document.getElementsByTagName('textarea');
    // For now hardwire to first element
    var text = texts[0];

    // And spawn the request
    var text_edit_id = text.getAttribute("edit_id");
    var edit_msg = {
	msg: "edit",
	text: text.value,
	id: text_edit_id
    };

    console.log("  findActiveTextArea:"+JSON.stringify(edit_msg));
    port.postMessage(edit_msg);
}

/* Message handling multiplexer */
function textareas_message_handler(msg, port) {
    console.log("textareas_message_handler: "+JSON.stringify(msg));

    // What was the bidding?
    var cmd = msg.msg;
    if (cmd == "find_edit") {
	findActiveTextArea();
    } else if (cmd == "update") {
	var id = msg.id;
	var content = msg.text;
	updateTextArea(id, content);
    } else {
	console.log("textareas_message_handler: un-handled message:"+cmd);
    }
}

// Hook up the incoming message handler for both return messages
// as well as direct messages from main extension.

port.onMessage.addListener(textareas_message_handler);
chrome.extension.onConnect.addListener(function(iport) {
	iport.onMessage.addListener(textareas_message_handler);
    });

/*
 editTextArea

 Called when the edit button on a page is clicked, once done
 it finds the appropriate text area, extracts it's text and
 fires a message to the main extension to trigger the editing
*/
 
function editTextArea(event) {
    var img = event.currentTarget;
    var edit_id = img.getAttribute("edit_id");
    console.log("editTextArea:"+edit_id);

    var texts = document.getElementsByTagName('textarea');

    for (var i=0; i<texts.length; i++) {
	var text = texts[i];

	var text_edit_id = text.getAttribute("edit_id");

	if (text_edit_id == edit_id)
	{
	    var edit_msg = {
		msg: "edit",
		text: text.value,
		id: edit_id
	    };
	    
	    console.log("  edit_msg:"+JSON.stringify(edit_msg));
	    port.postMessage(edit_msg);
	}
    }
}

function findTextAreas() {

    console.log("findTextAreas() running");
	   
    var texts = document.getElementsByTagName('textarea');
    var tagged = 0;

    for (var i=0; i<texts.length; i++) {
	var text = texts[i];

	// We don't want to tag all text boxen, especially if they are hidden
	var display = text.style.getPropertyCSSValue('display');
	if (display && display.cssText=="none")
	{
	    continue;
	}

	// Also skip textareas we have already tagged
	var existing_id = text.getAttribute("edit_id");
	if (existing_id)
	{
	    console.log("  skipping tagged textarea:" +existing_id);
	    continue;
	}

	// Set attribute of text box so we can find it
	var edit_id = "eta_"+page_edit_id;
	text.setAttribute("edit_id", edit_id);

	// Add a clickable edit img to trigger edit events
	var image = document.createElement('img');
	image.setAttribute("edit_id", edit_id);
	image.src = editImgURL;
	text.parentNode.insertBefore(image, text.nextSibling);
	image.addEventListener('click', editTextArea, false);

	// Inc
	page_edit_id = page_edit_id + 1;
	tagged = tagged + 1;
    }

    console.log("findTextAreas: tagged "+tagged+" boxes, page_edit_id now "+page_edit_id);
}

/*
 We want to search for text areas when the page is first loaded as well as after any additional
 XHR events made by the page which may load additional elements
*/

// Called when content script loaded
findTextAreas();
console.log("textareas.js loaded: "+document.readyState);
