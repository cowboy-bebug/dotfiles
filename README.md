```bash
sudo apt install \
  git \
  python3-pip python3.8-dev \
  npm
```

```bash
FONT=Hack
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/${FONT}.zip
unzip ${FONT}.zip -d .local/share/fonts
fc-list | grep ${FONT}
```

```bash
stow -Rv $(ls -d */ | tr "/\n" " ")
```
