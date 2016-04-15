#!/bin/bash
diff <(./$1) <(./$2)
exit $?
