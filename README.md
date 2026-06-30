# rc

Personal runtime configuration files.

## Setting up `yabai` + `skhd` (macOS tiling window manager and hotkey daemon)

### Restarting the services

``` sh
yabai --stop-service
yabai --start-service

skhd --stop-service
skhd --start-service
```

Check that the services are actually running:

``` sh
pgrep -fl yabai
pgrep -fl skhd
```

If any one of them doesn't run, follow the troubleshooting below:

### Check macOS permissions

Go to System Settings → Privacy & Security → Accessibility and make sure both `yabai` and `skhd` are
enabled.

### Check whether `skhd` is blocked by Secure Keyboard Entry

``` sh
tail -n 80 /tmp/skhd_$USER.err.log
```

If you see something like "secure keyboard entry is enabled", we need to first close the app that's
enabling Secure Keyboard Entry. Find the PID of that app using:

``` sh
ioreg -l -w 0 | grep kCGSSessionSecureInputPID
```

Look for string "kCGSSessionSecureInputPID=X" and run (note: the `-ww` remove width limit; otherwise
the output might be truncated):

``` sh
ps -ww -p PID -o pid,comm,args
```

One example output:

``` sh
PID COMM             ARGS
620 /System/Library/ /System/Library/CoreServices/loginwindow.app/Contents/MacOS/loginwindow console
```

One fix for this example output that did succeed is to log out other users on this mac.

### Check `yabai`'s log

``` sh
tail -n 80 /tmp/yabai_$USER.err.log
tail -n 80 /tmp/yabai_$USER.out.log
```

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

`monitor-bell` keeps the tmux window alert flag enabled, while `bell-action any`
forwards BEL events to Alacritty. This preserves the yellow tab alert and lets
Alacritty run its configured bell command.

Alacritty is configured to play the macOS Blow sound on terminal bell output:

```toml
[bell]

command = { program = "afplay", args = ["/System/Library/Sounds/Blow.aiff"] }
```

The goal is to keep that audible bell and the tmux yellow window alert without
letting a background BEL make the Alacritty Dock icon bounce. Alacritty starts
through a small shell wrapper that emits `CSI ? 1042 l`, disabling urgency hints
for bell events before launching tmux:

```toml
[terminal.shell]

program = "/bin/sh"
args = ["-c", "printf '\\033[?1042l'; exec /opt/homebrew/bin/tmux new-session -A -s main"]
```
