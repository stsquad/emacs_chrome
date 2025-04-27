/* -*- tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * This "content" script finds TextArea's in the DOM and tags them
 * with a unique ID and edit button. When the button is
 * clicked it communicates with the master extension page to send an
 * edit request.
 *
 * This file is part of emacs_chrome (http://github.com/stsquad/emacs_chrome)
 * and licensed under the GPLv3. See the COPYING file for details
 */

// Shim for Firefox compatibility
if (typeof browser !== 'undefined') {
    var browser_runtime = browser.runtime;
    var browser_sendMessage = browser.runtime.sendMessage;
} else {
    browser_runtime = chrome.extension;
    browser_sendMessage = chrome.extension.sendRequest;
}

var port = browser_runtime.connect();

// For findTextAreas
var page_edit_id = 0;
var pageTextAreas = [];

// via options
var enable_button = true;
var enable_dblclick = false;
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
	if (title === null || title.length === 0) {
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
  getEmptyMessage

  Return a partially filled message hash with details.
 */
function getEmptyMessage()
{
    var empty_msg = {
        pageUrl: document.URL,
        title: getTitle(),
    };
    return empty_msg;
}
/*
  getEditButton

  Return an edit button
*/
function getEditButton(id)
{
    var button = document.createElement('div');
    button.setAttribute("class", "ewe_edit_button");
    button.setAttribute("id", id);
    button.setAttribute("edit_id", id);
    button.setAttribute("style", 'background-image: url('+chrome.extension.getURL("icons/gumdrop.png")+");");
    button.addEventListener('click', editTextArea);
    return button;
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

    console.log("new textAreaTracker %d", page_edit_id);

    // The text areas event handlers we attach
    this.focusListener = setFocused;
    this.editEvent = editTextArea;

    this.text.addEventListener('focus',  this.focusListener);

    if (enable_dblclick)
        this.text.addEventListener('dblclick', this.editEvent);

    // The edit button
    if (enable_button) {
        this.button = getEditButton(this.edit_id);
        this.text.parentNode.insertBefore(this.button, text.nextSibling);
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
            // fix for some sites that maintain and submit a shadow copy of the text area
            // that gets updated on "input" events to the real one; e.g. fastmail.fm's
            // "Compose Message" page.
            this.text.dispatchEvent(new Event("input"));
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
    if ( (t.position().left + t.width() < 0) ||
         (t.position().top + t.height() < 0) ) {
        console.log("tagTextArea: skipping offscreen text area %s (%d/%d x %d/%d)",
                    t.attr("id"),
                    t.position().left, t.width(), t.position().top, t.height());
        return;
    }

    // If spellcheck is turned off, usually it's just for quick editing, e.g. To: fields in gmail
    var spellcheck = t.attr("spellcheck");
    if (spellcheck && spellcheck == "false" &&
        t.prop("tagName") === "TEXTAREA") {
        console.log("tagTextArea: skipping spellcheck disabled textarea");
        return;
    }

    // No edit for read-only text
    // This also removes annoying edit button that appears under the menu bar in Google Docs viewer.
    if (text.readOnly) {
        console.log("tagTextArea: skipping readOnly text");
        return;
    }

    var existing_id = text.getAttribute("edit_id");
    if (!existing_id) {
        // tag it
        pageTextAreas.push(new textAreaTracker(text));
    } else {
        console.log("Warning: found existing ID when tagging: "+existing_id);
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
        if (event.initTextEvent !== undefined) {
            event.initTextEvent('textInput', true, true, null, '\n', 0);
            tracker.text.dispatchEvent(event);
        }

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
	var edit_msg = getEmptyMessage();
    edit_msg.msg = "edit";
    edit_msg.text = text_tracker.getContent();
	edit_msg.id = text_tracker.edit_id;
    console.debug("sendTextArea: sending message:", edit_msg);
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
            var no_txt_msg = getEmptyMessage();
            no_txt_msg.msg = "error";
            no_txt_msg.text = "No textarea in focus in: "+getTitle();
            no_txt_msg.orig_cmd = "find_edit";
            port.postMessage(no_txt_msg);
        }
    };

    setFocused = function(){
        // Update UI?
        var id = this.getAttribute("edit_id");
        focusedEdit = getTextAreaTracker(id);
        if (focusedEdit !== undefined) {
            var focus_msg = getEmptyMessage();
            focus_msg.msg = "focus";
            focus_msg.id = id;
            port.postMessage( focus_msg );
            this.addEventListener('blur',	function() {
                var unfocus_msg = getEmptyMessage();
                unfocus_msg.msg = "focus";
                unfocus_msg.id = null;
                port.postMessage( unfocus_msg );
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
    } else {
        console.log("No text area found");
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

    imgSummary.removed.forEach(function(e) {
        console.log("button was removed: " + e.getAttribute("edit_id"));
    });

    // Process all new textareas
    var allAdded = taSummary.added.concat(div1Summary.added, div2Summary.added);
    allAdded.forEach(function(e) {
        // If the area was duplicated we want to remove its ID
        if (e.getAttribute("edit_id")) {
            e.removeAttribute("edit_id");
        }
        tagTextArea(e);
    });

    // Handle removed elements
    var allRemoved = taSummary.removed.concat(div1Summary.removed, div2Summary.removed);
    allRemoved.forEach(function(e) {
        if (e.getAttribute("edit_id")) {
            console.log("tagged element removed: " + e.getAttribute("edit_id"));
        }
    });
}

/*
 * Selectors for the various things that might be editable. Add new ones here.
 */

var editable_selectors = [ "textarea",
                           "div[contenteditable='true']",
                           "div[contenteditable='plaintext-only']" ];

/* Message handling multiplexer */
function localMessageHandler(msg, port) {
    // What was the bidding?
    var cmd = msg.msg;
    if (cmd == "config") {
        console.log("config response: "+msg);
        enable_button = msg.enable_button;
        enable_dblclick = msg.enable_dblclick;
        enable_debug = msg.enable_debug;

        var all = editable_selectors.join(",");
        $(all).each(function() {
            tagTextArea(this);
        });

        /*
         * The mutation summary is responsible for monitoring all
         * changes to the page and triggering updates.
         *
         */
        var textarea_observer = new MutationSummary({
            callback: handleUpdatedElements,
            queries: [
                {   // we don't want the source page accidently duplicating our tags
                    element: "div[class='ewe_edit_button']"
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
browser_runtime.onConnect.addListener(function(iport) {
    iport.onMessage.addListener(localMessageHandler);
});

console.debug("About to fetch our configuration from the background process");

/*
  To start the whole process off we first need to fetch our configuration
  from the background process.
*/
port.postMessage({msg: "config"});

console.debug("About to install contextmenu event listener on document");

// Inform the background process whenever the user opens
// the context menu on an editable element.
document.addEventListener("contextmenu", (function(event) {
    var elem = event.srcElement;
    console.debug("contextmenu event called on elem: ", elem);
    if (elem && elem.getAttribute("edit_id")) {
        var edit_msg = getEmptyMessage();
        edit_msg.msg = "edit";
        edit_msg.text = elem.value;
        edit_msg.id = elem.getAttribute("edit_id");
        var request = {
            type: "menu_target",
            edit_msg: edit_msg
        };
        browser_sendMessage(request);
    }
}));

console.debug("Edit with Emacs has finished initializing");
