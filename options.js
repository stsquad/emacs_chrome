/*
 * options.js
 *
 * This is the JavaScript for the extension options page
 *
 * This file is part of emacs_chrome (http://github.com/stsquad/emacs_chrome)
 * and licensed under the GPLv3. See the COPYING file for details
 */

// Saves options to localStorage.
function save_options() {
  var port_box = document.getElementById("port");
  var port = port_box.value;
  localStorage["edit_server_port"] = port;

  // Update status to let user know options were saved.
  var status = document.getElementById("status");
  status.innerHTML = "Options Saved.";
  setTimeout(function() {
    status.innerHTML = "";
  }, 750);
}

// Restores select box state to saved value from localStorage.
function restore_options() {
  var port = localStorage["edit_server_port"];
  if (!port) {
    port = 9292;
  }
  var port_box = document.getElementById("port");
  port_box.value = port;
}

