# Useful Commands

## Change macOS hostname

```sh
sudo scutil --set HostName <hostname>
sudo scutil --set LocalHostName <hostname>
sudo scutil --set ComputerName <hostname>
```

## Open Sublime Text 3 with terminal

```sh
sudo mkdir -p /usr/local/bin
sudo ln -s "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" /usr/local/bin/subl
```