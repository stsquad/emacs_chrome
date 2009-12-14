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

function editTextArea(edit_id) {
    console.log("editTextArea");
    
    alert ("Editing "+edit_id);
}
 
function findTextAreas() {

    console.log("Finding text area");

    text_id=0;
    
    // Try the jQuery way
    $('textarea').each ( function() {
	console.log("each="+$(this));
	id_string="eta_"+text_id;
	$(this).attr({ emacs_chrome_id: id_string });
	$(this).after('<a href="javascript:editTextArea(\''+id_string+'\')">'+editImgTag+'</a>');
	text_id++;
    } )
	    
}

findTextAreas();
