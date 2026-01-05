[![CI](https://github.com/stsquad/emacs_chrome/actions/workflows/test.yml/badge.svg)](https://github.com/stsquad/emacs_chrome/actions/workflows/test.yml)
[![MELPA](https://melpa.org/packages/edit-server-badge.svg)](https://melpa.org/#/edit-server)
[![MELPA Stable](https://stable.melpa.org/packages/edit-server-badge.svg)](https://stable.melpa.org/#/edit-server)

About
=====

Edit with Emacs is an addon for webextension compatible browsers like
Opera or Firefox that allows you to edit text areas on your browser in
a more full featured editor. It does this in conjunction with an "Edit
Server" which services requests by the browser. This is because
extensions cannot spawn new processes as a security measure.

The extension packages a native elisp version that can be downloaded,
just follow the instructions from the options page of the extension.
It has been known to work with GNU Emacs and Aquamacs (MacOS); it is
presently not compatible with XEmacs.

Other example edit servers can be found at the project homepage. There
is no reason why other server scripts could not spawn other editors
and currently a number of servers support the simple URL based
protocol.

This extension is licensed under the GPL v3 and development versions
can be found at: http://github.com/stsquad/emacs_chrome

Installing
==========

As mentioned above there are two parts you will need. The edit server
which needs to be installed and configured on Emacs and the browser
extension itself.

Edit Server
-----------

Perhaps the easiest way to install edit-server.el is via
[MELPA](https://melpa.org/#/edit-server) which provides a range of
packages that can be installed via the package manager:

    M-x package-install edit-server

Once installed you will want to update your configuration so the
server is started before you edit. For example using the popular
[use-package](https://github.com/jwiegley/use-package) config
framework you might do something like this:

    (use-package edit-server
      :ensure t
      :commands edit-server-start
      :init (if after-init-time
                  (edit-server-start)
                (add-hook 'after-init-hook
                          #'(lambda() (edit-server-start))))
      :config (setq edit-server-new-frame-alist
                    '((name . "Edit with Emacs FRAME")
                      (top . 200)
                      (left . 200)
                      (width . 80)
                      (height . 25)
                      (minibuffer . t)
                      (menu-bar-lines . t)
                      (window-system . x))))

Please see the built-in help for more information on how to configure
the edit servers behaviour.

Browser Extension
-----------------

If you just want to install Edit with Emacs you can simply visit
addons.mozilla.org (AMO) at:

https://addons.mozilla.org/en-US/firefox/addon/edit-with-emacs1/

You then have the option of installing the packaged edit-server from the
options page or alternatively you can install the latest version from
MELPA if you have it enabled in Emacs 24+.

If you would like to help with the development of Edit with Emacs the
easiest way is to fork the github repository (https://github.com/stsquad/emacs_chrome)
and clone it to your development system. Then in Chrome(ium) go to:

Tools->Extensions
Select "Developer Mode"
Click "Load Unpacked Extension"

and point it at the cloned repository.

Hacking
=======

This modeline should be used in every source file to keep indentation
consistent:

    // -*- tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-

This tells both emacs use spaces instead of tabs and use a basic indentation
of 4 spaces.

There is also a universal .editorconfig which should enforce this.

The code has a fair amount of whitespace damage. Please don't submit
mega-patches to clean it up, just fixup the local code as you go. It
makes history harder to navigate as well as potentially introducing
changes in the noise.

There is currently a minimal GitHub Actions workflow which essentially
ensures the edit-server.el still compiles. Additions to the test
coverage are always welcome. ;-)

Submitting to Store
===================

* Update NEWS
* Test
* Update version in manifest.json
* Tag vX.Y
* git archive --format zip --output emacs_chrome_X.Y.zip vX.Y
* Upload that

History
=======

Dave Hilley [wrote the original proof of concept](https://web.archive.org/web/20130719135014/http://www.thegibson.org/blog/archives/689)
that showed it could be done. [I](http://www.bennee.com/~alex) then hacked around with the Javascript
to make the behaviour more like the classic It's All Text add-on available to Firefox.

This extension used to work with Chrome(ium) but since the migration
away from Manifest v2 it no longer works and has been removed from the
extension store.

Authors
=======

Edit with Emacs is open source and is brought to you thanks to
contributions from the following people:

David Hilley
Alex Bennée
Riccardo Murri
Niels Giesen
Wei Hu
Ævar Arnfjörð Bjarmason
Chris Houser
Robert Goldman
Phil Pennock
Sudish Joseph
IRIE Shinsuke
Nelson Elhage
David J. Biesack
Christopher Browne
Mark Shroyer
Remco van 't Veer
John Croisant
Tim Cuthbertson
Ryszard Szopa
Kazuya Sakakihara
Steve Purcell
Dale Sedivec
Jonas Bernoulli
Le Wang
Mike Shulman
Matt Walker
Aaron Schrab
Adam Spiers
Dato Simó
Philippe Vaucher
Eli Barzilay
Marc Tamsky
Attila Lendvai
Daniel Kraus
John Brzustowski

Other Code
==========

This extensions takes advantage of the
[jQuery library](http://jquery.com/) to do a lot of the heavy lifting
in searching the page for elements.

It uses John Resig's
[jQuery Colour Animation](https://github.com/jquery/jquery-color)
plug-in to do the colour fades of the elements.

The settings code uses Frank Kohlhepp's excellent fancy-settings
library. This has since
[been pulled from the web](https://github.com/frankkohlhepp/fancy-settings).

The textarea code uses the rather nifty
[mutation summary library](https://github.com/rafaelw/mutation-summary)
by Google.
