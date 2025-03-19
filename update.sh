#!/bin/bash

# emacs
cp ~/.emacs emacs
for file in emacs.d/*.{el,org}; do
	echo "$(basename "$file")"
	cp ~/.emacs.d/$(basename $file) emacs.d/
done

# tmux
cp ~/.tmux.conf tmux.conf

# alacritty
cp ~/.config/alacritty/alacritty.toml alacritty.toml

# zsh
cp ~/.zshrc zshrc
cp ~/.zshrc-ssh zshrc-ssh

# ssh
cp ~/.ssh/config ssh-config
