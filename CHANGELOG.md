# Changelog

Todos los cambios relevantes de este repositorio se documentan en este archivo.

Este formato sigue la convencion de [Keep a Changelog](https://keepachangelog.com/es-ES/1.1.0/).

## [2026-02-11]

### Added

- Se agrego `modules/terminal/alacritty-system.nix` para centralizar la configuracion compartida de Alacritty en perfiles Linux no-NixOS.
- Se agrego `README.md` con descripcion de perfiles, estructura del repositorio, comandos de uso y convenciones de mantenimiento.

### Changed

- `ubuntu.nix` ahora importa `modules/terminal/alacritty-system.nix` y elimina la configuracion duplicada de Alacritty.
- `contribute-clippy.nix` ahora importa `modules/terminal/alacritty-system.nix` y elimina la configuracion duplicada de Alacritty.
- `git/git.nix` simplifica su firma de modulo eliminando el argumento no utilizado `userSettings`.
- `emacs/emacs.nix` simplifica su firma de modulo eliminando el argumento no utilizado `aikenMode`.
- `sh/zsh.nix` unifica `COMPACT_HOME` en `/home/antonio/compact_binaries` para consistencia entre shell runtime y session variables.
- `sh/zsh.nix` corrige `home.sessionPath` para agregar solo `"$COMPACT_HOME"` en lugar de una ruta invalida con `"$HOME:$COMPACT_HOME"`.

### Fixed

- Se evita una posible falla de evaluacion de Home Manager por argumentos de modulo no inyectados desde el flake en `git/git.nix` y `emacs/emacs.nix`.
- Se elimina conflicto de configuracion Bash/Zsh quitando `programs.bash.enable` y `programs.bash.profileExtra` de `google-chrome/google-chrome.nix`.

### Security

- Se removio `--no-sandbox` de los comandos de lanzamiento de Chrome en `google-chrome/google-chrome.nix`.
