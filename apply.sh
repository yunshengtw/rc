#!/bin/bash

# emacs
cp emacs ~/.emacs
for file in emacs.d/*; do
	cp $file ~/.emacs.d/$(basename $file)
done

# vim
cp vimrc ~/.vimrc

# tmux
mkdir -p ~/.tmux/themes/
git clone https://github.com/nordtheme/tmux.git ~/.tmux/themes/nord-tmux 2>/dev/null
cp tmux/tmux.conf ~/.tmux.conf
cp tmux/tmux.bind ~/.tmux.bind
cp tmux/tmux.unbind ~/.tmux.unbind

# alacritty
mkdir -p ~/.config/alacritty
cp alacritty.toml ~/.config/alacritty/alacritty.toml

# zsh
cp zshrc ~/.zshrc
cp zshrc-cmd ~/.zshrc-cmd
cp zshrc-ssh ~/.zshrc-ssh

# ssh
cp ssh-config ~/.ssh/config

# skhd
cp skhdrc ~/.skhdrc

# gemini
cp gemini/settings.json ~/.gemini/settings.json
mkdir -p ~/.gemini/policies/
for file in gemini/policies/*.toml; do
	cp $file ~/.gemini/policies/$(basename $file)
done
