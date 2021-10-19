#!/usr/bin/env bash

dir=~/dotfiles         # dotfiles directory
olddir=~/dotfiles_old  # old dotfiles backup directory

# list of files/dirs to symlink to in ~
files="lein emacs.d bash_custom gitconfig gitignore"

# list of files/dirs to symlink to in ~/.config
configs="flake8"

echo "Using $olddir for dotfiles backup"
mkdir -p $olddir

cd ~

for file in $files; do
    if [ -h ".$file" ]; then
        echo "~/.$file is already symlink to `readlink -f .$file`"
    else
        if [ -e ".$file" ]; then
            echo "Moving ~/.$file to $olddir for backup"
            mv ~/.$file $olddir
        fi
        echo "Creating symlink to $dir/$file in ~"
        ln -s $dir/$file ~/.$file
    fi
done

for file in $configs; do
    if [ -h ".config/$file" ]; then
        echo "~/.config/$file is already symlink to `readlink -f .config/$file`"
    else
        if [ -e ".config/$file" ]; then
            echo "Moving ~/.config/$file to $olddir for backup"
            mv ~/.$file $olddir
        fi
        echo "Creating symlink to $dir/$file in ~/.config"
        ln -s $dir/$file ~/.config/$file
    fi
done
