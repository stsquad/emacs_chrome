/*
 * TextArea.js
 *
 * This "content" script finds TextArea's in the DOM and tags them
 * with a unique ID and edit button. When the button is
 * clicked it communicates with the master extension page to send an
 * edit request.
 *
 */
 
var editImgURL = chrome.extension.getURL("gumdrop.png");
var port = chrome.extension.connect();
 
/*
 updateTextArea

 Called when we want to update the text area with our updated text
*/
function updateTextArea(edit_id) {
    console.log("updateTextArea");
}

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
	    content = text.value;
	    console.log("  content:"+content);
	    port.postMessage({message: "edit_request", values: content});
	}
    }
}

function findTextAreas() {

    console.log("Finding text area (Pure HTML Version)");
	   
    var texts = document.getElementsByTagName('textarea');

    for (var i=0; i<texts.length; i++) {
	var text = texts[i];

	// We don't want to tag all text boxen, especially if they are hidden
	var display = text.style.getPropertyCSSValue('display');
	if (display && display.cssText=="none")
	{
	    continue;
	}

	// Set attribute of text box so we can find it
	var edit_id = "eta_"+i;
	text.setAttribute("edit_id", edit_id);

	// Add a clickable edit img to trigger edit events
	var image = document.createElement('img');
	image.setAttribute("edit_id", edit_id);
	image.src = editImgURL;
	text.parentNode.insertBefore(image, text.nextSibling);
	image.addEventListener('click', editTextArea, false);
    }
}

	    
findTextAreas();
