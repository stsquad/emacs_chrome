/*
 * xmlcomms.js
 *
 * This handles making XMLHttp calls to the remote end
 *
 */

 
  // Called when the user clicks on the browser action.
  chrome.browserAction.onClicked.addListener(function(tab) {
    console.log("Thingy clicked!");
    //    chrome.tabs.executeScript(tab.id, {file: "update.js", allFrames: false});
    console.log("Run update");
  });
