# dotfiles

The repo uses [stow](https://www.gnu.org/software/stow) to symlink config files.
Here's how it's run:

```sh
stow -Rv $(ls -d */ | tr "/\n" " ") --target=$HOME
```
