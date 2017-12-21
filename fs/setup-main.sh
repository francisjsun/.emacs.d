#!/bin/bash

function fs_apt_get
{
    echo "**** installing $1 ****"

    checkName=$1
    if ! [ -z "$2" ]
    then
	checkName=$2
    fi

    if ! command -v $checkName 1>/dev/null
    then
	printf "y\n" | apt-get install $1
    fi

    echo "**** $1 installed ****"
}

fs_apt_get clang

fs_apt_get global
