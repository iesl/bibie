#!/bin/bash
# run_class.sh [<jvm options>] <scala object with main> [<arguments>]

args="$@"

memory=2g

PROJ_ROOT=$BIBROOT

# If the root directory is not passed as an environment variable, set it to 
# the current directory.
if [ -z "$PROJ_ROOT" ]
then
  PROJ_ROOT=`pwd`
fi

echo "PROJ_ROOT: "$PROJ_ROOT

# Get classpath: all jar files from maven -- either from a file CP.hack, or 
# if this file does not exist yet, from the output of a maven compile.
# TODO: there should be a nice and automated way for starting after building -
# for now, this hack attempts that, somehow.
if [ ! -f "$PROJ_ROOT/CP.hack" ]
then
 echo 'CP.hack does not exist, get class paths ...'
 cd $PROJ_ROOT
 mvn compile -X \
 | grep 'classpathElements = ' \
 | sed 's#^.* classpathElements = \[\(.*\)\]$#\1#g' \
 | sed 's#, #:#g' \
 | head -1 \
 > $PROJ_ROOT/CP.hack
 cd -
 echo '... done'
fi

CP=`cat $PROJ_ROOT/CP.hack`

java -Dfile.encoding=UTF8 -cp $CP -Xmx$memory $args

