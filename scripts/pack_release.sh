#!/bin/bash

# This file is released under terms of BSD license
# See LICENSE file for more information

function show_help(){
  echo "$0 -v <version_number> [-o <output_dir>] [-b <branch-name>]"
  echo ""
  echo "Options:"
  echo " -v <version_number> Version number given to the release"
  echo " -o <output_dir>     Specify where to place the release archive (default: current directory)"
  echo " -b <branch-name>    Specifiy a branch to use for the release (default: master)"
}

# Define local variable
CLAW_BRANCH="master"
CLAW_MAIN_REPO="git@github.com:C2SM-RCM/claw-compiler.git"
CLAW_RELEASE_DIR=""
CLAW_ANT_ROOT="omni-cx2x/src"
CLAW_VERSION=""

while getopts "hb:o:v:" opt; do
  case "$opt" in
  h)
    show_help
    exit 0
    ;;
  o)
    CLAW_RELEASE_DIR=$OPTARG
    ;;
  b)
    CLAW_BRANCH=$OPTARG
    ;;
  v)
    CLAW_VERSION=$OPTARG
  esac
done

if [[ ! $CLAW_VERSION ]]
then
  echo "Error: version is mandatory"
  show_help
  exit 1
fi

CLAW_VERSION_NAME="claw-compiler-$CLAW_VERSION"

echo ""
echo "================================================="
echo "CLAW FORTRAN Compiler release archive information"
echo "================================================="
echo "- Repo: $CLAW_MAIN_REPO"
echo "- Branch: $CLAW_BRANCH"
echo "- Dest dir: $CLAW_RELEASE_DIR"
echo "- Version: $CLAW_VERSION"
echo ""

rm -rf $CLAW_VERSION_NAME # Make sure destination doesn't exists
git clone --depth 1 -b $CLAW_BRANCH $CLAW_MAIN_REPO $CLAW_VERSION_NAME
cd $CLAW_VERSION_NAME
git submodule init
git submodule update --remote
# TODO update version number in CMakeLists.txt
cd $CLAW_ANT_ROOT
ant -Dantfile.dir=$(pwd) common.bootstrap
ant -Dantfile.dir=$(pwd) common.resolve
cd ../../..
rm -f $CLAW_VERSION_NAME.tar*
tar cvf $CLAW_VERSION_NAME.tar $CLAW_VERSION_NAME/*
gzip $CLAW_VERSION_NAME.tar
[[ $CLAW_RELEASE_DIR ]] && mv $CLAW_VERSION_NAME.tar.gz $CLAW_RELEASE_DIR
rm -rf $CLAW_VERSION_NAME
