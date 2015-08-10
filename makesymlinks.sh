#!/bin/bash
############################
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles         # dotfiles directory
olddir=~/dotfiles_old  # old dotfiles backup directory

# list of files/folders to symlink in homedir
files="lein emacs.d"

##########

echo "Using $olddir for dotfiles backup"
mkdir -p $olddir

cd ~
for file in $files; do
    if [ -h ".$file" ]; then
        echo "~/.$file is already symlink to `readlink -f $file`"
    else
        if [ -e ".$file" ]; then
            echo "Moving ~/.$file to $olddir for backup"
            mv ~/.$file $olddir
        fi
        echo "Creating symlink to $dir/$file in home directory."
        ln -s $dir/$file ~/.$file
    fi
done

