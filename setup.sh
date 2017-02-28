#!/bin/sh
# #       Filename: setup.sh
# #  Creation Date: Saturday, 23 January 2016 01:02 PM AEDT
# #  Last Modified: Tuesday, 28 February 2017 10:15 AM AEDT
# #         Author: Tim Cross <theophilusx AT gmail.com>
# #    Description:
# #

cd ~/.emacs.d

echo "Tangle the file $*"

/usr/local/bin/emacs -q -batch -l setup.el --eval "(tangle-my-init \"$*\")"
