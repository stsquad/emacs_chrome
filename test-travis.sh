#!/bin/sh
#
# Very simple test script

set -ex

# About the simplest test we can do, ensure the file loads without error
${EMACS} --version
${EMACS} -q --batch -l servers/edit-server.el


