/*
 * TextArea.js
 *
 * This "content" script finds TextArea's in the DOM and wraps them in
 * a unique <div> block with and edit button. When the button is
 * clicked it communicates with the master extension page to send an
 * edit request.
 *
 * This uses jQuery for handy stuff
 */


function findTextAreas() {

    console.log("Finding text area");

    var result = null;

    // Find the body text area
    result = document.getElementsByTagName('textarea');
  
    if(result) {
	console.log("Found some areas:"+result);
	for (ta in result)
	{
	    console.log("Processing one:"+ta);
	}
    }

    // Try the jQuery way
    result = $('textarea');
    if (result) {
	console.log("Found some via jQuery:"+result);
    }
	    
}

findTextAreas();
