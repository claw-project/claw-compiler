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

shellcheck -V
if ! shellcheck offline.sh
then
  exit 1
fi
if ! shellcheck pack_release.sh
then
  exit 1
fi
cd ../driver/bin || exit 1
if ! shellcheck ../etc/claw_f.conf
then
  exit 1
fi
if ! shellcheck ../libexec/claw_f_lib.sh.in
then
  exit 1
fi
if ! shellcheck clawfc.in ../libexec/claw_f_lib.sh.in ../etc/claw_f.conf.in
then
  exit 1
fi
cd - || exit 1
