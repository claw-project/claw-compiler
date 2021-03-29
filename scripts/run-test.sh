#!/bin/bash
# This file is released under terms of BSD license
# See LICENSE file for more information
# author: Mikhail Zhigun

CMD=$1
REPORT_FILE=$2

if $CMD ; then
    cat ${REPORT_FILE}
    exit 0
else
    cat ${REPORT_FILE}
    exit 1
fi
