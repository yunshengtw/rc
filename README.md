# rc

Personal runtime configuration files.

## Nord tmux Theme

This repo vendors the runtime files from the Nord tmux theme:

https://github.com/nordtheme/tmux

The current copy was taken from:

- Revision: `f7b6da07ab55fe32ee5f7d62da56d8e5ac691a92`
- Describe: `v0.1.0-42-gf7b6da0`

Only the files needed by tmux at runtime are kept under `tmux/themes/nord-tmux`:

- `nord.tmux`
- `src/nord.conf`
- `src/nord-status-content.conf`
- `src/nord-status-content-no-patched-font.conf`
- `license`

### Updating From Upstream

To refresh the vendored copy, clone the upstream repository separately:

```sh
git clone https://github.com/nordtheme/tmux.git /tmp/nord-tmux
```

Then copy only the runtime files back into this repo:

```sh
cp /tmp/nord-tmux/nord.tmux tmux/themes/nord-tmux/nord.tmux
cp /tmp/nord-tmux/license tmux/themes/nord-tmux/license
cp /tmp/nord-tmux/src/nord.conf tmux/themes/nord-tmux/src/nord.conf
cp /tmp/nord-tmux/src/nord-status-content.conf tmux/themes/nord-tmux/src/nord-status-content.conf
cp /tmp/nord-tmux/src/nord-status-content-no-patched-font.conf tmux/themes/nord-tmux/src/nord-status-content-no-patched-font.conf
```

After copying, reapply the local changes below and update this README and
`tmux/themes/nord-tmux/UPSTREAM.md` with the new upstream revision.

### Local Changes

The window status format is changed so tmux bell alerts color inactive window tabs yellow.

The patched files are:

- `tmux/themes/nord-tmux/src/nord-status-content.conf`
- `tmux/themes/nord-tmux/src/nord-status-content-no-patched-font.conf`

Both files keep the original Nord window status layout, but make the inactive window colors
conditional on tmux's `window_bell_flag` format. When a window has a bell alert, the tab uses a
yellow background and black text. When the window is selected, tmux clears the bell flag and the tab
returns to the normal Nord colors.

The notification behavior itself is configured in `tmux/tmux.conf`:

```tmux
set-window-option -g monitor-bell on
set-option -g bell-action any
set-option -g visual-bell off
set-option -g window-status-bell-style 'fg=#2e3440,bg=#ebcb8b,bold'
```

Alacritty is configured to ignore terminal bell output:

```toml
[bell]

duration = 0
command = "None"
```
