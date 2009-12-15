/*
 * TextArea.js
 *
 * This "content" script finds TextArea's in the DOM and tags them
 * with a unique ID and edit button. When the button is
 * clicked it communicates with the master extension page to send an
 * edit request.
 *
 * This uses jQuery for handy stuff
 */
 
var editImgURL = chrome.extension.getURL("gumdrop.png");
var editImgTag = "<img src=\""+editImgURL+"\">";

/*
 updateTextArea

 Called when we want to update the text area with our updated text
*/
function updateTextArea(edit_id) {
    console.log("updateTextArea");
}

function editTextArea(event) {
    var img = event.currentTarget;
    var edit_id = img.getAttribute("edit_id");
    console.log("editTextArea:"+edit_id);
}

/*    
function findTextAreas() {

    console.log("Finding text area (jQuery Version)");

    text_id=0;
    
    // Try the jQuery way
    $('textarea').each ( function() {
	id_string="eta_"+text_id;
	$(this).attr({ emacs_chrome_id: id_string });
	$(this).after('<a id='+id_string+'\'></a>');
	// Get the anchor element
	a = $(this)[0].nextSibling;
	a.innerHTML='<img src="'+editImgURL+'>';
	a.addEventListener("click", id_string, editTextArea);
	text_id++;
    } )
	    
}
*/

function findTextAreas() {

    console.log("Finding text area (Pure HTML Version)");
	   
    var texts = document.getElementsByTagName('textarea');

    for (var i=0; i<texts.length; i++) {
	var text = texts[i];

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
