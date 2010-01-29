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
var pageTextAreas = [];
var findingTextAreas = false;

function updateEvent(thing, event, listener)
{
    // First remove the event so we don't stack up multiple ones
    try {
	thing && thing.removeEventListener(event, listener);
    } catch (err) {
	console.log ("event listener not registered for "+thing + "/"+event);
    }
    
    // And the update again
    if (thing) {
	thing.addEventListener(event, listener, false);
    }
}
 
/*
  textAreaTracker

  This object wraps up all the information about a given text area on the page.
  It allows us to update the listeners and query stuff from one central location.
*/

function textAreaTracker(text)
{
    this.edit_id = "eta_"+page_edit_id;
    page_edit_id = page_edit_id + 1;
    this.text = text;
    this.text.setAttribute("edit_id", this.edit_id);

    // The text areas event handlers we attach
    this.focusListener = setFocused;
    this.dblclickListener = function(){sendTextArea(this);};
    this.text.addEventListener('focus',  this.focusListener);
    this.text.addEventListener('dblclick', this.dblclickListener);
    
    // The img 
    this.image = document.createElement('img');
    this.image.style.cursor='pointer';
    this.image.setAttribute("edit_id", this.edit_id);
    this.image.src = editImgURL;

    this.clickListener = editTextArea;
    this.image.addEventListener('click', this.clickListener, false);

    this.text.parentNode.insertBefore(this.image, text.nextSibling);

    // The update function removes and re-adds events
    this.updateEvents = function()
    {
	updateEvent(this.text, 'focus', this.focusListener);
	updateEvent(this.text, 'dblclick', this.dblclickListener);
	updateEvent(this.image, 'click', this.clickListener);
    }
}

 
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

    var existing_id = text.getAttribute("edit_id");
    if (!existing_id)
    {
	// tag it
	var tat = new textAreaTracker(text);
	pageTextAreas.push(tat);
    } else {
	// Even though this text has an edit_id it might not actually be the
	// text we think it is. If the textAreaTracker.text doesn't match then
	// we should tag this with something different
	for (var i=0; i<pageTextAreas.length; i++) {
	    var existing_area = pageTextAreas[i];
	    if ( (existing_area.edit_id == existing_id) &&
		 (existing_area.text != text ) )
	    {
		console.log("found a duplicate id!");
		// OK, first things first, find any images that think
		// they are associated with a text area and remove them
		siblings = text.parentElement.childNodes;
		console.log("has "+siblings.length+ " siblings");

		for (var j=0; j<siblings.length; j++) {
		    if (! (siblings[j].getAttribute == undefined) ) {
			console.log("B doing sibling: "+siblings[j].toString());
		
			    if ( (siblings[j].getAttribute("edit_id") == existing_area.edit_id) &&
				 (siblings[j].toString() == "[object HTMLImageElement]") ) {
				console.log("yoink");
				siblings[j].parentElement.removeChild(siblings[j]);
			    }
			}
		}
		
		// And create a new tracked text area
		new textAreaTracker(text);
	    }
	}
    }
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
  sendTextArea

  Send the text area to the main part of the extension to be passed
  on to the external editor. Eventually 
*/

function sendTextArea(text) {
    var text_edit_id = text.getAttribute("edit_id");
    // And spawn the request
    var edit_msg = {
	msg: "edit",
	text: text.value,
	id: text_edit_id
    };
    port.postMessage(edit_msg);
}

/*
  Handle focused text area
*/
(function(){
     var focusedEdit = null;

     findActiveTextArea = function() {
	 if (focusedEdit) {
	     sendTextArea(focusedEdit);
	 } 
     };

     setFocused = function(){
	 focusedEdit = this;		
     };
 })();

/* Message handling multiplexer */
function localMessageHandler(msg, port) {
    // What was the bidding?
    var cmd = msg.msg;
    if (cmd == "find_edit") {
	findActiveTextArea();
    } else if (cmd == "update") {
	var id = msg.id;
	var content = msg.text;
	updateTextArea(id, content);
    } else {
	console.log("localMessageHandler: un-handled message:"+cmd);
    }
}

// Hook up the incoming message handler for both return messages
// as well as direct messages from main extension.

port.onMessage.addListener(localMessageHandler);
chrome.extension.onConnect.addListener(function(iport) {
	iport.onMessage.addListener(localMessageHandler);
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
	    sendTextArea(text);
	}
    }
}

function findTextAreasInDocument(doc) {
    var texts;
    try {
	texts = doc.getElementsByTagName('textarea');
	for (var i=0; i<texts.length; i++) {
	    tagTextArea(texts[i]);
	}
    } catch (err) {
	// it seems some (I)FRAMES have undefined contentDocuments
	console.log("findTextAreasInDocument: failed with "+err);
    }
}

function findTextAreas() {

    // Don't run through this if already finding stuff, lest we trigger events
    if (findingTextAreas)
	return;

    findingTextAreas = true;

    findTextAreasInDocument(document);

    // IFRAMEs
    var iframes = document.getElementsByTagName('iframe');
    for (i = 0; i < iframes.length; i++) {
	findTextAreasInDocument(iframes[i].contentDocument);
    }

    // FRAMEs
    var frames = document.getElementsByTagName('frame');
    for (i = 0; i < frames.length; i++) {
	findTextAreasInDocument(frames[i].contentDocument);
    }

    /* This may not be needed
    // And finally lets update any events and ensure they have event listeners
    for (var i=0; i<pageTextAreas.length; i++) {
	pageTextAreas[i].updateEvents();
    }
    */

    findingTextAreas = false;
    return true;
}

/*
 We want to search for text areas when the page is first loaded as well
 as after any additional XHR events made by the page which may load
 additional elements (such as Gmail).
*/

/* called when content script loaded */
findTextAreas();

/* called upon further document mods */
document.addEventListener("DOMNodeInserted", (function () {
    findTextAreas();
    return true;
}), false);
