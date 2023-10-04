#!/bin/bash

usage=0
silent=0
unsup=0

while getopts 'shu' opt; do
  case "$opt" in
    h) echo -e "USAGE\n -h: get usage\n -s: disable error explanation\n -u: unsupported samples even tested"
    usage=1
      ;;
    s) silent=1
      ;;
    u) unsup=1
      ;;
    ?) usage=1
  esac
done

if [ ${usage} == 0 ]
then
  echo -e "  ________________________________"
  echo -e " /                                \\"
  echo -e "|                                  |"
  echo -e "|           \033[1;36mGLaDOS Tests\033[0m           |"
  echo -e "|                                  |"
  echo -e " \\________________________________/"
  echo

  if [ ${silent} == 1 ]
  then
      echo "[ Silent mode ]"
      echo
  fi

  nbr=0
  nbr_passed=0

  name="-> \[mini-sh]: "
  name_exit="-> \[mini-sh]: exit"

  Red='\033[0;31m'
  NoColor='\033[0m'
  Green='\033[0;32m'

  FILES="./test/samples/*"
  mkdir /tmp/GLaDOS_Tests
  for f in $FILES
  do
    ((nbr=nbr+1))
    NAME=${f##*/}
    printf "Testing [%-15s] file:" $NAME
    ./glados < $f > /tmp/GLaDOS_Tests/current_glados
    scheme -q < $f > /tmp/GLaDOS_Tests/current_scheme
    DIFF=$(diff /tmp/GLaDOS_Tests/current_glados /tmp/GLaDOS_Tests/current_scheme) 
    if [ "$DIFF" == "" ] 
    then
      ((nbr_passed=nbr_passed+1))
      echo -e "$2\t\t${Green}PASSED${NoColor}"
    else
      echo -e "$2\t\t${Red}NOT PASSED${NoColor}"
      if [ $silent == 0 ]
        then
        diff -y /tmp/GLaDOS_Tests/current_glados /tmp/GLaDOS_Tests/current_scheme
      fi
    fi
  done

  if [ ${unsup} == 1 ]
  then
    echo
    echo "[ Usupported samples testing ]"
    echo
    FILES="./test/unsupported_samples/*"
    for f in $FILES
    do
      ((nbr=nbr+1))
      NAME=${f##*/}
      printf "Testing [%-15s] file:" $NAME
      ./glados < $f > /tmp/GLaDOS_Tests/current_glados
      scheme -q < $f > /tmp/GLaDOS_Tests/current_scheme
      DIFF=$(diff /tmp/GLaDOS_Tests/current_glados /tmp/GLaDOS_Tests/current_scheme) 
      if [ "$DIFF" == "" ] 
      then
        ((nbr_passed=nbr_passed+1))
        echo -e "$2\t\t${Green}PASSED${NoColor}"
      else
        echo -e "$2\t\t${Red}NOT PASSED${NoColor}"
        if [ $silent == 0 ]
          then
          diff -y /tmp/GLaDOS_Tests/current_glados /tmp/GLaDOS_Tests/current_scheme
        fi
      fi
    done
  fi

  echo
  echo  Tests passed [${nbr_passed} / ${nbr}]

  rm -rf /tmp/GLaDOS_Tests

  if [ $nbr_passed == $nbr ]; then
    exit 0
  else
    exit 1
  fi
fi
