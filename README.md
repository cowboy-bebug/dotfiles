### Link:
```
git clone --recurse-submodules git@github.com:cowboy-bebug/dotfiles.git
./bootstrap
```

### [Generate a new GPG key](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/generating-a-new-gpg-key), ignore and set the key:
```
git update-index --assume-unchanged git/.gitconfig
vim git/.gitconfig
```

### Install vscode extensions after installing the `code` binary from vscode:
```
./vscode-extensions
```
