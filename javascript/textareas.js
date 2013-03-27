// -*- tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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

var editImgURL = chrome.extension.getURL("icons/gumdrop.png");
var port = chrome.extension.connect();

// For findTextAreas
var page_edit_id = 0;
var pageTextAreas = [];

// via options
var enable_button = true;
var enable_dblclick = false;
var enable_keys = false;
var enable_debug = false;

// Decorate console.log so that it only logs
// when the enable_debug setting is true
var orig_console_log = console.log;
console.log = function() {
    if (enable_debug) {
        orig_console_log.apply(console, Array.prototype.slice.call(arguments));
    }
};

/*
  getTitle

  Get the title from the DOM, if that fails try to synthesise something sensible
*/

function getTitle()
{
	var title = document.title;
	if (title == null || title.length==0) {
		try {
			title = document.getElementsByTagName('title').item().innerText;
		} catch (err) {
			console.log ("failed to extract title from HTML ("+err+")");
			title = document.documentURI;
		}
	}
	return title;
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
	this.editEvent = editTextArea;
	this.keydownListener = function (e) {
		// Alt-Enter
		if (e.altKey && e.keyCode == 13)
			editTextArea(e);
	};
	
	this.text.addEventListener('focus',  this.focusListener);

	if (enable_dblclick)
		this.text.addEventListener('dblclick', this.editEvent);
	if (enable_keys)
		this.text.addEventListener('keydown', this.keydownListener);

	// The img 
	if (enable_button) {
		this.image = document.createElement('img');
		this.image.setAttribute("class", "ewe_edit_button");
		this.image.setAttribute("edit_id", this.edit_id);
		this.image.src = editImgURL;
		this.image.addEventListener('click', editTextArea);
		this.text.parentNode.insertBefore(this.image, text.nextSibling);
	}

	// Some methods to get and set content
	this.getContent = function () {
		if (this.text.tagName == "DIV") {
			return this.text.innerHTML;
		} else {
			return this.text.value;
		}
	};

	this.setContent = function(new_text) {
		if (this.text.tagName == "DIV") {
			this.text.innerHTML = new_text;
		} else {
			this.text.value = new_text;
		}
	};
	
}

/*
  getTextAreaTracker

  Fetch the appropriate textAreaTracker for the given ID. As it tracks each element we
  care about it's probably worth going through this short array rather than iterating
  the entire DOM every time.

  Currently the array is not cleaned when elements are removed from the page.
*/

function getTextAreaTracker(search_id)
{
	for (var i=0; i<pageTextAreas.length; i++) {
		if (pageTextAreas[i].edit_id == search_id)
			return pageTextAreas[i];
	}

	return null;
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
    /*
      Don't bother with hidden fields unless hidden
      by the parent container. If they are tagging
      with our gumdrop won't be an issue and when the block
      is restored we'll be able to edit
    */
    var t = $(text);
    if (t.is(":hidden")) {
        if (!t.parent().is(":hidden")) {
            console.log("tagTextArea: skipping :hidden textarea");
            return;
        }
    }

    // Is it offscreen (like some github textareas)
    if (t.position().left + t.width() < 0) return;
    if (t.position().top + t.height() < 0) return;

    // If spellcheck is turned off, usually it's just for quick editing, e.g. To: fields in gmail
    var spellcheck = text.getAttribute("spellcheck");
    if (spellcheck && spellcheck == "false") return;

    // No edit for read-only text
    // This also removes annoying edit button that appears under the menu bar in Google Docs viewer.
    if (text.readOnly) return;

    var existing_id = text.getAttribute("edit_id");
    if (!existing_id)
    {
	// tag it
	pageTextAreas.push(new textAreaTracker(text));
    } else {
        /*
         * TODO: remove?
         * 
         * This code is possibly redundant now page changes are tracked using mutation events
         * and we take steps to remove duplicate tags at that point. However we might as well
         * keep the code for now and see if anything triggers it in the field.
         */
        console.log("Found existing ID when tagging: "+existing_id);
	var existing_area = getTextAreaTracker(existing_id);
	if ( existing_area &&
	     (existing_area.text != text ) )
	{
	    console.log("tagTextArea: Working around a duplicate id!");
	    // OK, first things first, find any images that think
	    // they are associated with a text area and remove them
	    siblings = text.parentElement.childNodes;

	    for (var j=0; j<siblings.length; j++) {
		if (! (siblings[j].getAttribute == undefined) ) {
		    if ( (siblings[j].getAttribute("edit_id") == existing_area.edit_id) &&
			 (siblings[j].toString() == "[object HTMLImageElement]") ) {
			siblings[j].parentElement.removeChild(siblings[j]);
		    }
		}
	    }

	    // And create a new tracked text area
	    pageTextAreas.push(new textAreaTracker(text));
	}
    }
}

/*
  updateTextArea

  Called when we want to update the text area with our updated text
*/
function updateTextArea(id, content) {
    var tracker = getTextAreaTracker(id);
    if (tracker) {
	tracker.setContent(content);
	var orig = $(tracker.text).css('background-color');
	$(tracker.text).css({'background-color': 'yellow'});
	// mark node as changed
	var event = document.createEvent("HTMLEvents");
	event.initEvent('change', true, false);
	tracker.text.dispatchEvent(event);

	// set selection to after end of the text
	tracker.text.selectionStart = content.length;
	// send a textInputEvent to append a newline
	event = document.createEvent("TextEvent");
	event.initTextEvent('textInput', true, true, null, '\n', 0);
	tracker.text.dispatchEvent(event);

	window.setTimeout(function() {
            // reset the text to the original without newline
            tracker.setContent(content);
	    $(tracker.text).animate({ 'backgroundColor': orig }, 2000);
	}, 100);
    }
}

/*
  sendTextArea

  Send the text area to the main part of the extension to be passed
  on to the external editor. Eventually 
*/

function sendTextArea(text_tracker) {
	// And spawn the request
	var edit_msg = {
		msg: "edit",
		text: text_tracker.getContent(),
		title: getTitle(),
		id: text_tracker.edit_id
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
		} else {
			var msg_text = "No textarea in focus in: "+getTitle();
			port.postMessage( {msg: "error", text: msg_text} );
		}
	};

	setFocused = function(){
		// Update UI?
		var id = this.getAttribute("edit_id");
		focusedEdit = getTextAreaTracker(id);
		if (focusedEdit != undefined) {
			port.postMessage( {msg: "focus", id: id} );
			this.addEventListener('blur',	function() {
				port.postMessage( {msg: "focus", id: null} );
				this.removeEventListener('blur',arguments.callee,false);
			});
		} else {
			console.log ("setFocused: failed to find a tracker for "+id);
        }
	};
})();

/*
  editTextArea

  Called when various edit events are called (dblClick, key-stroke or img click)

  It finds the edit id, finds the textTracker for the id and sends the
  tracker off to the be sent to the edit system.
*/

function editTextArea(event) {
	var element = event.currentTarget;
	var edit_id = element.getAttribute("edit_id");
	var tracker = getTextAreaTracker(edit_id);
	if (tracker) {
		sendTextArea(tracker);
	}
}

/*
  This is the main find function for searching for TEXTAREAS and other related divs
*/

function findTextAreas(elements) {
	console.log("findTextAreas: running over "+elements.length);

    for (var i=0; i<elements.length; i++) {
        var x = $(elements[i]);
	    // Process textareas
	    var texts = x.find("textarea");
	    for (var j=0; j<texts.length; j++) {
		    tagTextArea(texts[j]);
	    }

	    // lets see if we can find any contenteditable stuff
	    var editable = x.find("div[contenteditable='true']");
	    for (var j=0; j<editable.length; j++) {
		    tagTextArea(editable[j]);
	    }

	    // According to http://www.w3.org/TR/html5/editing.html#contenteditable
	    // we should only see true/false or inherit. However G+ seems to ignore
	    // that so lets look for those here.
	    var editable = x.find("div[contenteditable='plaintext-only']");
	    for (var j=0; j<editable.length; j++) {
		    tagTextArea(editable[j]);
	    }
    }
}

/*
  This triggers each time an element is changed.
*/
function handleUpdatedElements(summaries) {
    // These are in order of the queries in MutationSummary call
    var imgSummary = summaries[0];
    var taSummary = summaries[1];
    var div1Summary = summaries[2];
    var div2Summary = summaries[3];

    // Clean up duplicate img tags
    imgSummary.added.forEach(function(e) {
        $(e).remove();
    });

    // Process all new textareas
    var allAdded = taSummary.added.concat(div1Summary.added, div2Summary.added);
    allAdded.forEach(function(e) {
        // If the area was duplicated we want to remote it's ID
        if (e.getAttribute("edit_id")) {
            e.removeAttribute("edit_id");
        }
        tagTextArea(e);
    });

    // For now just log removed
    // TODO: something cleaner...
    var allRemoved = taSummary.removed.concat(div1Summary.removed, div2Summary.removed);
    allRemoved.forEach(function(e) {
        if (e.getAttribute("edit_id")) {
            console.log("tagged element removed, we should probably do something about that");
        }
    });
}


/* Message handling multiplexer */
function localMessageHandler(msg, port) {
	// What was the bidding?
	var cmd = msg.msg;
	if (cmd == "config") {
		console.log("config response: "+msg);
		enable_button = msg.enable_button;
		enable_dblclick = msg.enable_dblclick;
		enable_keys = msg.enable_keys;
		enable_debug = msg.enable_debug;
		findTextAreas([$('*')]);

        /* 
         * The mutation summary is responsible for monitoring all
         * changes to the page and triggering updates.
         *
         */
        var textarea_observer = new MutationSummary({
            callback: handleUpdatedElements,
            queries: [
                {   // we don't want the source page accidently duplicating our tags
                    element: "img[class='ewe_edit_button']"
                },
                {
                    element: "textarea"
                },
                {
                    element: "div[contenteditable='true']"
                },
                {
                    element: "div[contenteditable='plaintext-only']"
                }
            ]
        });


	} else if (cmd == "find_edit") {
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
  To start the whole process off we first need to fetch our configuration
  from the background process.
*/
port.postMessage({msg: "config"});


// Inform the background process whenever the user opens
// the context menu on an editable element.
document.addEventListener("contextmenu", (function(event) {

	var elem = event.srcElement;
	if (elem && elem.getAttribute("edit_id")) {
	    var request = {
		    type: "menu_target",
		    edit_msg: {
		        msg: "edit",
		        text: elem.value,
		        title: getTitle(),
		        id: elem.getAttribute("edit_id")
		    }
	    };
	    chrome.extension.sendRequest(request);
	}

}));
