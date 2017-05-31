#!/bin/bash

shellcheck offline.sh
shellcheck pack_release.sh
cd driver/bin || exit 1
shellcheck ../etc/claw_f.conf
shellcheck ../libexec/claw_f_lib.sh.in
shellcheck clawfc.in ../libexec/claw_f_lib.sh.in ../etc/claw_f.conf.in
cd - || exit 1
