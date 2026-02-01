#!/bin/bash

# emacs
cp ~/.emacs emacs
EMACS_FILES="early-init.el iris.el ysc-theme.el cheat-key.org"
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
cp ~/.zshrc-cmd zshrc-cmd
cp ~/.zshrc-ssh zshrc-ssh

# ssh
cp ~/.ssh/config ssh-config

# skhd
cp ~/.skhdrc skhdrc

# gemini
cp ~/.gemini/settings.json gemini/settings.json
GEMINI_POLICIES="deny-git-add-commit.toml"
for file in $GEMINI_POLICIES; do
	cp ~/.gemini/policies/$file gemini/policies/
done

