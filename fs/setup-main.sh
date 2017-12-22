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

# removed because debian's distibute is too old
# see this issue of ggtags
# https://github.com/leoliu/ggtags/issues/31
# fs_apt_get global

# build global 6.6.1 from source
if ! command -v global 1>/dev/null
then
    echo "**** installing global-6.6.1 ****"
    wget http://tamacom.com/global/global-6.6.1.tar.gz
    tar -xzvf global-6.6.1.tar.gz
    cd global-6.6.1
    ./configure
    make
    make install
    echo "**** global-6.6.1 installed ****"
fi

