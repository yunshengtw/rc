#!/bin/bash

# emacs
cp emacs ~/.emacs
for file in emacs.d/*; do
	cp $file ~/.emacs.d/$(basename $file)
done

# tmux
cp tmux/tmux.conf ~/.tmux.conf
cp tmux/tmux.bind ~/.tmux.bind
cp tmux/tmux.unbind ~/.tmux.unbind

# alacritty
mkdir -p ~/.config/alacritty
cp alacritty.toml ~/.config/alacritty/alacritty.toml

# zsh
cp zshrc ~/.zshrc
cp zshrc-ssh ~/.zshrc-ssh

# ssh
cp ssh-config ~/.ssh/config
