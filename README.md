### Create symlinks:

```
git clone git@github.com:cowboy-bebug/dotfiles.git
./bootstrap
```

### Set and ignore a GPG key:

```
git config --global user.signingkey <GPG_SIGNING_KEY>
git update-index --assume-unchanged git/.gitconfig
```

### Install vscode extensions after installing the `code` binary from vscode:

```
./vscode-extensions
```
