/*
 * TextAreas.js
 *
 * This "content" script finds TextArea's in the DOM and tags them
 * with a unique ID and edit button. When the button is
 * clicked it communicates with the master extension page to send an
 * edit request.
 *
 * This file is part of emacs_chrome (http://github.com/stsquad/emacs_chrome)
 * and licensed under the GPLv3. See the COPYING file for details
 */
 
var editImgURL = chrome.extension.getURL("gumdrop.png");
var port = chrome.extension.connect();
var page_edit_id = 0;

/*
  tagTextArea

  Any textarea we edit needs to be tagged with a unique ID so
  when the get the edit response we know what to fill in. This
  gets called several times not least as some text boxes can
  appear in the document after first load.
*/
function tagTextArea(text)
{
    // We don't want to tag all text boxen, especially if they are hidden
    var display = text.style.getPropertyCSSValue('display');
    if (display && display.cssText=="none")
    {
	return;
    }

    // Also skip textareas we have already tagged
    var existing_id = text.getAttribute("edit_id");
    if (existing_id)
    {
	return;
    }

    // Set attribute of text box so we can find it
    var edit_id = "eta_"+page_edit_id;
    text.setAttribute("edit_id", edit_id);
    text.addEventListener('focus', setFocused);

    // Add a clickable edit img to trigger edit events
    var image = document.createElement('img');
    image.setAttribute("edit_id", edit_id);
    image.src = editImgURL;
    text.parentNode.insertBefore(image, text.nextSibling);
    image.addEventListener('click', editTextArea, false);

    // Inc
    page_edit_id = page_edit_id + 1;
}
  

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
(function(){
     var focusedEdit = null;

     findActiveTextArea = function() {
	 var text;

	 if (focusedEdit) {
	     text = focusedEdit;
	 } 
	     
	 tagTextArea(text);

	 // And spawn the request
	 var text_edit_id = text.getAttribute("edit_id");
	 var edit_msg = {
	     msg: "edit",
	     text: text.value,
	     id: text_edit_id
	 };
	 port.postMessage(edit_msg);
     };

     setFocused = function(){
	 focusedEdit = this;		
     };
 })();

/* Message handling multiplexer */
function textareas_message_handler(msg, port) {
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
	    
	    port.postMessage(edit_msg);
	}
    }
}

function findTextAreas() {
    var texts = document.getElementsByTagName('textarea');

    for (var i=0; i<texts.length; i++) {
	var text = texts[i];
	tagTextArea(text);
    }

    return true;
}

/*
 We want to search for text areas when the page is first loaded as well as after any additional
 XHR events made by the page which may load additional elements
*/

/* called when content script loaded */
findTextAreas();

/* called upon further document mods */
document.addEventListener("DOMNodeInserted", (function () {
    findTextAreas();
    return true;
}), false);
