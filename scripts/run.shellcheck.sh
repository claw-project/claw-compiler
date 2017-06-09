#!/bin/bash
#
# This file is released under terms of BSD license
# See LICENSE file for more information
#
# Continuous integration script to check all the bash scripts of the project.
# It uses shellcheck to check the consitency of the scripts.
#
# author: clementval
#

shellcheck offline.sh
shellcheck pack_release.sh
cd ../driver/bin || exit 1
shellcheck ../etc/claw_f.conf
shellcheck ../libexec/claw_f_lib.sh.in
shellcheck clawfc.in ../libexec/claw_f_lib.sh.in ../etc/claw_f.conf.in
cd - || exit 1
