#!/bin/sh
# #       Filename: setup.sh
# #  Creation Date: Saturday, 23 January 2016 01:02 PM AEDT
# #  Last Modified: Saturday, 30 January 2016 11:00 AM AEDT
# #         Author: Tim Cross <theophilusx AT gmail.com>
# #    Description:
# #

cd ~/.emacs.d
rm init.el

emacs -batch -l setup.el
