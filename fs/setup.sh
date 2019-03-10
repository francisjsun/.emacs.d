#!/bin/bash

echo "emacs dependencies seting up..."

unameOut=$(uname -s)

function fancy-exit
{
    read -p "press ENTER to continue."
    exit $1
}


# mac setup
if [[ "$unameOut" == "Darwin"* ]]
then
    setup_dir=$(dirname "$0")
    open -a Terminal $setup_dir/setup-darwin.sh 
    fancy-exit 0
fi

# windows setup
if [[ "$unameOut" == "MINGW"* ]]
then
    sh -e ~/.emacs.d/fs/setup-windows.sh
    fancy-exit 0
fi

# other sys(ubuntu...)

if [ "$EUID" -ne 0 ]
then
    echo "should run as root"
    fancy-exit 1
fi

fs_terminal=0

echo "finding available terminal"
if command -v gnome-terminal 1>/dev/null
then
    fs_terminal=gnome-terminal
fi

if command -v xterm 1>/dev/null
then
    fs_terminal=xterm
fi

if command -v $fs_terminal 1>/dev/null
then
    $fs_terminal -- bash ~/.emacs.d/fs/setup-other.sh
else
    echo "no available terminal found"
    fancy-exit 1
fi

echo "emacs dependencies setted done"

read -p "press ENTER to continue."
