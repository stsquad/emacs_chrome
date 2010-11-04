(function(){

  var edit_msg = {};

  function menuClicked(info, tab) {
    if (edit_msg) {
      var tab_port = chrome.tabs.connect(tab.id);
      tab_port.tab = tab;
      handleContentMessages(edit_msg, tab_port);
    }
  }


  var menu_enabled = false;

  function enableContextMenu() {
    if (!menu_enabled) {
      chrome.contextMenus.removeAll();
      chrome.contextMenus.create({
        title: "Edit with Emacs",
        contexts: ["editable"],
        onclick: function(info, tab) {
          menuClicked(info, tab);
        }
      });
      menu_enabled = true;
    }
  }

  function disableContextMenu() {
    chrome.contextMenus.removeAll();
    menu_enabled = false;
  }

  // Initialize the context menu based on stored options.
  if (localStorage.enable_contextmenu === "true") {
    enableContextMenu();
  } else {
    disableContextMenu();
  }


  function processRequest(request, sender, sendResponse) {
    if (request.type === "menu_target") {
      edit_msg = request.edit_msg;
    } else if (request.type === "enable_contextmenu") {
      if (request.enabled) {
        enableContextMenu();
      } else {
        disableContextMenu();
      }
    }
    sendResponse({});
  }

  chrome.extension.onRequest.addListener(processRequest);

})();