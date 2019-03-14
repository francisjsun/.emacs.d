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

# fs_apt_get clang

fs_apt_get libclang-dev

# removed because debian's distibute is too old
# see this issue of ggtags
# https://github.com/leoliu/ggtags/issues/31
# fs_apt_get global

# build global 6.6.1 from source
# if ! command -v global 1>/dev/null
# then
#     echo "**** installing global-6.6.1 ****"
#     wget http://tamacom.com/global/global-6.6.1.tar.gz
#     tar -xzvf global-6.6.1.tar.gz
#     cd global-6.6.1
#     ./configure
#     make
#     make install
#     cd ..
#     rm -rfv global*
#     echo "**** global-6.6.1 installed ****"
# fi

# rtags 
# if ! command -v rdm 1>/dev/null
# then
#     echo "**** installing rtags dependencies ****"
#     fs_apt_get cmake
#     fs_apt_get pkg-config
#     fs_apt_get bash-completion
#     fs_apt_get zlib1g-dev
#     fs_apt_get libssl-dev
#     echo "**** rtags dependencies installed ****"
    
#     echo "**** installing rtags-2.16 ****"
#     wget https://andersbakken.github.io/rtags-releases/rtags-2.16.tar.gz
#     tar -xzvf rtags-2.16.tar.gz
#     cd rtags-2.16
#     mkdir build
#     cd build
#     cmake ..
#     make
#     make install
#     cd ../../
#     rm -rfv rtags*
#     echo "**** rtags-2.16 installed ****"
# else
#     echo "**** rtags-2.16 installed ****"
# fi


