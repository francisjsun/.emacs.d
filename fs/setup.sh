#!/bin/bash

echo "emacs dependencies seting up..."

if [ "$EUID" -ne 0 ]
then
    echo "should run as root"
    exit 1
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
    $fs_terminal -e ~/.emacs.d/fs/setup-main.sh
else
    echo "no available terminal found"
    exit 1
fi

echo "emacs dependencies setted done"
