/* -*- tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 */

// Inform the background process whenever the user opens
// the context menu on an editable element.
document.addEventListener("contextmenu", (function(event) {
    var request = {
        type: "page_source",
        edit_msg: {
            msg: "edit",
            text: document.all[0].innerHTML,
            title: getTitle(),
            id: ("page_"+(new Date().getTime()))
        }
    };
    chrome.extension.sendRequest(request);
}));
