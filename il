#!/bin/bash
DIR=`dirname $0`
emacs -l $DIR/init.el -nw "$@"




