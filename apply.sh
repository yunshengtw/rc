#!/bin/bash

# emacs
cp emacs ~/.emacs
cp -r emacs.d ~/emacs.d

# tmux
cp tmux.conf .tmux.conf

# alacritty
mkdir -p ~/.config/alacritty
cp alacritty.toml ~/.config/alacritty/alacritty.toml

# zsh
cp zshrc ~/.zshrc
cp zshrc-ssh ~/.zshrc-ssh

# ssh
cp ssh-config ~/.ssh/config
