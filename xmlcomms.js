/*
 * xmlcomms.js
 *
 * This handles making XMLHttp calls to the remote end
 *
 */
 
  // Called when the user clicks on the browser action.
  chrome.browserAction.onClicked.addListener(function(tab) {
    console.log("Thingy clicked!");
    chrome.tabs.executeScript(tab.id, {file: "textareas.js", allFrames: false});
    console.log("Run update");
  });


  function handleContentMessages(msg)
  {
      console.log("handleContentMessages called:"+msg);
  }

  function contentTalking(port)
  {
      console.log("contentTalking:"+port);
      port.onMessage.addListener(handleContentMessages);
  }

  // Hook up whenever someone connects to the extension comms port
  chrome.extension.onConnect.addListener(contentTalking);

