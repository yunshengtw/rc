#!/bin/bash

# emacs
EMACS_FILES="early-init.el iris.el cheat-key.org"

cp ~/.emacs emacs
for file in $EMACS_FILES; do
	cp ~/.emacs.d/$file emacs.d/
done

# vim
cp ~/.vimrc vimrc

# tmux
cp ~/.tmux.conf tmux/tmux.conf
cp ~/.tmux.bind tmux/tmux.bind
cp ~/.tmux.unbind tmux/tmux.unbind

# alacritty
cp ~/.config/alacritty/alacritty.toml alacritty.toml

# zsh
cp ~/.zshrc zshrc
cp ~/.zshrc-ssh zshrc-ssh

# ssh
cp ~/.ssh/config ssh-config

# skhd
cp ~/.skhdrc skhdrc
