#!/bin/bash

echo "******** begin emacs dependencies setting up ********"

unameOut=$(uname -s)

# mac setup
if [[ "$unameOut" == "Darwin"* ]]
then
    setup_dir=$(dirname "$0")
    open -a Terminal $setup_dir/setup-darwin.sh 
    return 0
fi

# windows setup
if [[ "$unameOut" == "MINGW"* ]]
then
    sh -e ~/.emacs.d/fs/setup-windows.sh
    return 0
fi

# other sys(ubuntu...)

# if [ "$EUID" -ne 0 ]
# then
#     echo "should run as root"
#     return 0
# fi

PASSWORD=$1
source ~/.emacs.d/fs/setup-other.sh $PASSWORD

echo "******** end of emacs dependencies setting up ********"
