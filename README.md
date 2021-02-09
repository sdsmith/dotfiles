# Installation

```sh
git clone <repo_link> ~/.dotfiles

# For full suite (zsh based)
./zsh_install

# For basic tools et al
./install
```

Note: `install` is a subset of what's configured by `zsh_install`, and `zsh_install` depends on `install`.

The output will warn you if you are missing anything that is needed as part of setup. Add what is missing yourself (how will depend on the OS and distro) and then re-run the script until it's happy :).

Make sure to install the fonts  on the system hosting your terminal that are used as part of the configuration. See the "Font" section.

# Fonts
- [Liberation Mono](https://www.fontsquirrel.com/fonts/liberation-mono) (the one with the dotted zero)
- [MesloLGS NF](https://github.com/romkatv/powerlevel10k#automatic-font-installation)
