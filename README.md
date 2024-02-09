# dotfiles

The repo uses [stow](https://www.gnu.org/software/stow) to symlink config files.
Here's how it's run:

```sh
stow -Rv $(ls -d */ | tr "/\n" " ") --target=$HOME
```

To install dependencies from Brew:

```sh
brew bundle install --file=Brewfile --no-lock

# transient dependencies
brew bundle install --file=doom.Brewfile --no-lock
brew bundle install --file=zsh.Brewfile --no-lock
```
